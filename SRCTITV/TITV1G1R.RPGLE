000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080418     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080527     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080429     Ftitv1g1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080429     Ftitv1g1ps O    f  198        PRINTER usropn
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
003100080527     D fivabds       e ds                  extname(fivab00f)
003200080527     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003300080527     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
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
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000030715     D wGiro           s              1  0 INZ(*zeros)
006100070823     D wVATNOT_A       s                   LIKE(VATNOT)
006200060331     D wVATNOT_B       s                   LIKE(VATNOT)
006300060331     D wVATNOT_E       s                   LIKE(VATNOT)
006400030822     D*------------------
006500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006600030822     D*------------------
006700030822     D posDaDft        S              3  0 INZ(*zeros)
006800030822     D posADft         S              3  0 INZ(*zeros)
006900030822     D j               s              3  0 INZ(1)
007000030822     D wGiroDft        s              1  0 INZ(*zeros)
007100010330     D*------------------
007200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007300010330     D*------------------
007400010330     D WLBDA8          DS                  INZ
007500010330     D  G08DAT                 1      8  0
007600010330     D  G08INV                 9     16  0
007700010330     D  G08ERR                17     17
007800010330     D  G08TGI                18     22  0
007900080418     D*------------------
008000080418     D* Costanti
008100080418     D*------------------
008200080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400080418     D*------------------
008500080418     D* VARIABILI X "XCHKCHAR"
008600080418     D*------------------
008700080418     D TxtInOut        S           2048
008800080418     D ElencoChar      S            256
008900080418     D TipoElenco      S              1
009000080418     D CharSost        S              1
009100080418     D UpperCase       S              1
009200080418     D ChkNull         S              1
009300080418     D CharNull        S              1
009400080418     D EsitoBon        S              1
009500080418     D*------------------
009600010330
009700010330
009800010330
009900990915     C                   time                    wrkdata
010000990915     C                   time                    wrkora
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300990921     C                   reset                   wrkesito
010400010601     C*
010500010601     C                   exsr      opeini
010600010605     C                   exsr      rwvab
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011400010601     C*--------------------------------------------------------
011500010601     C     PREELA        BEGSR
011600010601     C*
011700010601     C* SE OCCORRE SPEDIRE IN FILIALE
011800010601     C                   if        invfil <> *zeros and
011900010601     C                             flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300000724     C*
012400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600010601     C     invfil        chain     azorg01l
012700000724     C                   if        %found
012800000724     C                   movel(p)  CMD(1)        depcmd
012900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013000000724     C*
013100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000724     C                   PARM                    depcmd
013500000724     C                   PARM                    LENGH
013600000724     C*
013700000724     C                   endif
013800000616     C*
013900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014000000616     C                   movel(p)  CMD(2)        depcmd
014100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014200000616     C*
014300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    depcmd
014700000616     C                   PARM                    LENGH
014800000613     C*
014900080429     C                   if        not %open(titv1g1ps)
015000080429     C                   open      titv1g1ps
015100070329     C                   except    testdett
015200000613     C                   endif
015300010601     C*
015400010601     C                   endif
015500010601     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200010601     C*--------------------------------------------------------
016300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016400010601     C*--------------------------------------------------------
016500010601     C     ENDELA        BEGSR
016600990915     C*
016700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016800040119     C*
016900040119     C* Effettuo la chiamata al CLLE preposto
017000040119     C                   call(e)   'TITVVTC'
017100040510     C                   parm                    parccm
017200040119     C                   parm                    parmbr
017300040119     C                   parm      '2'           paropz
017400050201     C*
017500050201     C* Effettuo lancio TISI95 solo x chiusura
017600050201     C                   CLEAR                   TISI95DS
017700050201     C                   EVAL      I95TLA = 'C'
017800050201     C                   CALL      'TISI95R'
017900050201     C                   PARM                    TISI95DS
018000000613     C*
018100080429     C                   if        %open(titv1g1ps)
018200000616     C                   except    findett
018300080429     C                   close     titv1g1ps
018400000613     C                   endif
018500000616     C*
018600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018700011113     C                   Z-ADD     150           LENGH            15 5
018800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018900000616     C                   PARM                    CMD(3)
019000000616     C                   PARM                    LENGH
019100000616     C*
019200010601     C                   ENDSR
019300010601     C***
019400010601
019500010601
019600010601
019700000613
019800000613     C*--------------------------------------------------------
019900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020000000613     C*--------------------------------------------------------
020100000621     C     STPR          BEGSR
020200000613     C*
020300080429     C                   if        not %open(titv1g1p)
020400080429     C   50              open      titv1g1p
020500990915     C                   endif
020600990915     C*
020700030715     C   50              except    riepilogo
020800990915     C*
020900080429     C                   if        %open(titv1g1p)
021000080429     C   50              close     titv1g1p
021100990914     C                   endif
021200000613     C*
021300000613     C                   ENDSR
021400000613     C***
021500000613
021600010601
021700010601
021800010330     C*--------------------------------------------------------
021900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022000010330     C*--------------------------------------------------------
022100010605     C     RWVAB         BEGSR
022200010330     C*
022300010330     C                   if        not %open(tivin00r)
022400010330     C                   open      tivin00r
022500010330     C                   endif
022600021025     C                   if        not %open(fivabwwr)
022700021025     C                   open      fivabwwr
022800010330     C                   endif
022900040119     C*
023000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023100040119     C                   exsr      prevat
023200040119     C*
023300040119     C                   if        chkcall = '0'
023400040119     C*
023500040119     C                   if        not %open(fivatwwr)
023600040119     C                   open      fivatwwr
023700040119     C                   endif
023800010330     C*
023900040119     C                   clear                   §CTROKVB          7 0
024000040119     C                   clear                   §CTROKVT          7 0
024100040119     C                   clear                   §CTRMO            7 0
024200040119     C                   clear                   §CTRNO            7 0
024300070823     C*
024400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024600080429     C                   exsr      inzvar
024700080429     C                   exsr      impvab
024800010330     C*
024900030822     C                   do        *HIVAL
025000010330     C*
025100030822     C                   read      tivin00r                               70
025200010618     C*
025300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025400010618     C                   if        vinflg = '1'
025500010618     C                   eval      flgOk = '1'
025600010618     C                   endif
025700040510     C*
025800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025900040510     C                   z-add     *zeros        wLenVINDTA        4 0
026000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026400040510     C                   z-add     1             wFlgVINDTA
026500040510     C                   leave
026600040510     C                   endif
026700040510     C                   enddo
026800010618     C*
026900040510     C                   if        vindta > *blanks AND
027000040510     C                             wFlgVINDTA = 1
027100010330     C                   add       1             rrnum
027200010330     C*
027300010601     C                   if        *in70 = *off and
027400010330     C                             (vinflg = *blanks
027500010330     C                              or vinflg = '0'
027600010330     C                              or vinflg = '2')
027700010330     C*
027800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027900010711     C                   if        vinflg = *blanks or vinflg = '0'
028000010711     C                   clear                   vinmsg
028100010711     C                   endif
028200010601     C*
028300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028400070925     C                   eval      vindta  = ' ' + %trim(vindta)
028500070925     C*
028600080527     C                   exsr      inzvar
028700010605     C                   exsr      impvab
028800010601     C*
028900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029000010605     C                   eval      depfil = VABLNP
029100010601     C                   exsr      repfil
029200010601     C                   if        depfil = invfil
029300021025     C                   if        vlrpoi = 999
029400030822     C                   move(P)   invfil        VABFGS
029500021025     C                   else
029600030822     C                   move(P)   vlrpoi        VABFGS
029700021025     C                   endif
029800010601     C*
029900010601     C                   exsr      PREELA
030000010601     C*
030100010604     C* Ebbene...
030200010604     C*
030300040119     C  N31              add       1             §CTROKVB
030400040119     C   32              add       1             §CTRMO
030500040119     C   31              add       1             §CTRNO
030600080418     C*
030700030822     C                   if        wGiro = 2
030800080513     C* Scrivo sempre il VAT
030900080513     C  N31              exsr      wrivat
031000080527     C                   exsr      chkvab
031100080513     C                   endif
031200010604     C*
031300010604     C                   if        *in31 = *off and
031400010604     C                             *in32 = *off
031500010604     C                   eval      vinflg = '1'
031600010604     C                   else
031700010604     C                   eval      recko = vindta
031800010604     C                   except    rigadett
031900010604     C                   eval      vinflg = '2'
032000010604     C                   endif
032100010604     C                   endif
032200010601     C*
032300010604     C                   endif
032400010604     C*
032500010330     C                   else
032600010330     C                   eval      vinflg = '1'
032700010330     C                   endif
032800010601     C*
032900010601     C  N70              update    tivin000
033000010330     C*
033100030822     C  N70              enddo
033200040119     C*
033300080527     C* Verifico dati rimasti "in cannna"...
033400080527     C                   seton                                        65
033500080527     C                   exsr      chkvab
033600080527     C*
033700040119     C                   endif                                                  (endif - chkcall)
033800010601     C*
033900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034000010601     C                   if        cntNonEl = *zeros or
034100010601     C                             flgMulti = '0'
034200010330     C* Se non ci sono record con errori ...
034300010601     C                   if        §ctrno = 0 and
034400010604     C                             §ctrmo = 0 and
034500010601     C                             flgStato <> '2'
034600010330     C* ... restituisco esito OK.
034700010330     C                   eval      wrkesito = '0'
034800010330     C                   else
034900040119     C                   if        §ctrokvb > 0
035000010330     C                   eval      wrkesito = '1'
035100010330     C                   else
035200010615     C                   if        flgOk = '0'
035300010615     C                   eval      wrkesito = '2'
035400010615     C                   else
035500010615     C                   eval      wrkesito = '6'
035600010615     C                   endif
035700010330     C                   endif
035800010330     C                   endif
035900010601     C                   else
036000010601     C                   eval      wrkesito = '9'
036100010601     C                   endif
036200010330     C*
036300010330     C                   if        %open(tivin00r)
036400010330     C                   close     tivin00r
036500010330     C                   endif
036600021025     C                   if        %open(fivabwwr)
036700021025     C                   close     fivabwwr
036800010330     C                   endif
036900040512     C                   if        %open(fivatwwr)
037000040512     C                   close     fivatwwr
037100040512     C                   endif
037200010601     C*
037300010601     C                   if        vlrpoi <> 999
037400010601     C                   eval      invfil = vlrpoi
037500010601     C                   endif
037600010330     C*
037700040119     C                   if        §ctrokvb > 0
037800010601     C                             and invfil > *zeros
037900010330     C                   exsr      invio
038000010330     C                   endif
038100010601     C*
038200010618     C                   if        flgGiro = '1'
038300010601     C                   exsr      endela
038400010618     C                   endif
038500010330     C*
038600010330     C                   ENDSR
038700010330     C***
038800080527
038900080527
039000080527
039100080527     C*----------------------------------------------------*
039200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
039300080527     C*----------------------------------------------------*
039400080527     C     CHKVAB        BEGSR
039500080527     C*
039600080527     C* Scrivo il VAB solo a rottura d spedizione
039700080527     C                   if        depspe = *zeros
039800080527     C  N31              eval      fivabds_sav = fivabds
039900080527     C                   eval      depspe = vabNSP
040000080527     C                   else
040100080527     C                   if        depspe = vabNSP AND *in65 = *off
040200080527     C                   add       VABNCL        sav_VABNCL
040300080527     C                   add       VABPKB        sav_VABPKB
040400080527     C                   else
040500080527     C  N31              eval      fivabds_ok = fivabds_sav
040600080527     C  N31              exsr      wrivab
040700080527     C  N31              eval      fivabds_sav = fivabds
040800080527     C                   eval      depspe = vabNSP
040900080527     C                   endif
041000080527     C                   endif
041100080527     C*
041200080527     C                   ENDSR
041300040119
041400040119
041500040119
041600040119     C*----------------------------------------------------*
041700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
041800040119     C*----------------------------------------------------*
041900040510     C     WRIVAB        BEGSR
042000080513     C*
042100080513     C* Gestisco l'eventuale rottura x numero spedizione
042200080527     C                   if        VABNSP <> depspe or *in65 = *on
042300070301     C                   if        wVATNOT_E <> *blanks
042400070301     C                   eval      VABCTM = '7Q'
042500070301     C                   endif
042600040119     C                   write     fivab000                                     => scarico il VAB
042700080513     C                   endif
042800040119     C*
042900040119     C                   ENDSR
043000040119
043100040119
043200040119
043300040119     C*----------------------------------------------------*
043400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
043500040119     C*----------------------------------------------------*
043600040510     C     WRIVAT        BEGSR
043700060512     C*
043800060512     C* Inizializzo i campi chiave
043900060512     C                   eval      VATFGS = VABFGS
044000060512     C                   eval      VATAAS = VABAAS
044100060512     C                   eval      VATCCM = VABCCM
044200070823     C                   eval      VATNRS = VABNRS
044300060512     C                   eval      VATNSP = VABNSP
044400060512     C                   eval      VATLNP = VABLNP
044500040714     C*
044600040714     C* Solo se VATNOT valorizzato scarico il buffer
044700040714     C                   if        VATNOT <> *blanks
044800040119     C*
044900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
045000040510     C                   if        VATTRC = *blanks
045100040510     C                   eval      VATTRC = 'E'
045200040510     C                   endif
045300040510     C                   write     FIVAT000
045400040119     C                   add       1             §CTROKVT
045500040714     C*
045600040714     C                   endif
045700060331     C*
045800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
045900060331     C                   if        wVATNOT_A <> *blanks
046000060331     C                   eval      VATNOT = wVATNOT_A
046100060331     C                   eval      VATTRC = 'A'
046200060331     C                   write     FIVAT000
046300060331     C                   add       1             §CTROKVT
046400060331     C                   endif
046500060331     C*
046600060331     C                   if        wVATNOT_B <> *blanks
046700060331     C                   eval      VATNOT = wVATNOT_B
046800060331     C                   eval      VATTRC = 'B'
046900060331     C                   write     FIVAT000
047000060331     C                   add       1             §CTROKVT
047100060331     C                   endif
047200060331     C*
047300060331     C                   if        wVATNOT_E <> *blanks
047400060331     C                   eval      VATNOT = wVATNOT_E
047500060331     C                   eval      VATTRC = 'E'
047600060331     C                   write     FIVAT000
047700060331     C                   add       1             §CTROKVT
047800060331     C                   endif
047900040119     C*
048000040119     C                   ENDSR
048100040119
048200040119
048300040119
048400040119     C*----------------------------------------------------*
048500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048600040119     C*----------------------------------------------------*
048700040119     C     PREVAT        BEGSR
048800040119     C*
048900040119     C* Compongo il nome del membro da dare al FIVATWWR
049000040119     C                   eval      parmbr = vlrhdl
049100040119     C                   movel     'M'           parmbr
049200040510     C                   eval      parccm = vlrksc
049300040119     C                   eval      paropz = '1'
049400040119     C* Effettuo la chiamata al CLLE preposto
049500040119     C                   call(e)   'TITVVTC'
049600040119     C                   parm                    parccm
049700040119     C                   parm                    parmbr
049800040119     C                   parm                    paropz
049900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050000040119     C                   if        %error
050100040119     C                   movel     '1'           chkcall
050200040119     C                   else
050300040119     C                   movel     '0'           chkcall
050400040119     C                   endif
050500040119     C*
050600040119     C                   ENDSR
050700010601
050800010601
050900010601
051000010330     C*----------------------------------------------------*
051100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051200010330     C*----------------------------------------------------*
051300040119     C     INZVAR        BEGSR
051400080527     C*
051500080527     C                   setoff                                       65
051600010330     C*
051700030822     C* Inizializzo variabili di wrk
051800020204     C                   Z-ADD     *zeros        Num5_0            5 0
051900060331     C                   MOVEL     *blanks       wVATNOT_A
052000060331     C                   MOVEL     *blanks       wVATNOT_B
052100060331     C                   MOVEL     *blanks       wVATNOT_E
052200030822     C*
052300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052400030822     C                   CLEAR                   SkSplitCSV
052500030822     C                   EVAL      i = 1
052600030822     C                   EVAL      posDa = *zeros
052700030822     C                   EVAL      posA  = *zeros
052800080527     C*
052900080527     C                   CLEAR                   FIVABDS
053000080527     C                   CLEAR                   FIVAT000
053100030822     C*
053200030822     C* Reimposto i valori di default
053300030822     C                   EXSR      DEFCAM
053400030822     C*
053500010330     C                   ENDSR
053600010330     C*----------------------------------------------------*
053700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053800010330     C*----------------------------------------------------*
053900010330     C     DEFCAM        BEGSR
054000030822     C*
054100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054200030715     C* e delimitatore testo.
054300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054600030822     C*
054700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
054900030715     C                   SETON                                        50
055000030715     C                   ELSE
055100030715     C                   SETOFF                                       50
055200030715     C                   ENDIF
055300030822     C*
055400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055500030822     C                   EVAL      posDaDft = 1
055600030822     C                   EVAL      posADft  = 0
055700030822     C                   EVAL      wGiroDft = 0
055800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055900030822     C                             posDaDft > 0
056000030822     C*
056100030822     C* Gestisco il 1° giro
056200030822     C                   IF        wGiroDft = 0
056300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056500030822     C* Incremento il contatore dei "giri"
056600030822     C                   EVAL      wGiroDft = 1
056700030822     C                   ELSE
056800030822     C                   EVAL      posDaDft = posADft
056900030822     C                   ENDIF
057000030822     C* Eseguo lo scan x trovare la fine del campo corrente
057100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057200030822     C*
057300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057500030822     C                   IF        posDaDft > 0 AND
057600030822     C                             posADft  > 0
057700030822     C* NCL
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'NCL'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiInt=*on
058700030822     C                   Z-ADD     PiVal         VABNCL
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* CCM
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'CCM'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiInt=*on
060000030822     C                   Z-ADD     PiVal         VABCCM
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* LNP
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'LNP'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300030822     C                   Z-ADD     PiVal         VABLNP
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* NRS
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'NRS'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiInt=*on
062600030822     C                   Z-ADD     PiVal         VABNRS
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* CTR
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CTR'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiInt=*on
063900030822     C                   Z-ADD     PiVal         VABCTR
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* PKB
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'PKB'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiNum=*on
065200030822     C                   Z-ADD     PiVal         VABPKB
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* VLB
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'VLB'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiNum=*on
066500030822     C                   Z-ADD     PiVal         VABVLB
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* QFT
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'QFT'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiNum=*on
067800030822     C                   Z-ADD     PiVal         VABQFT
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* CBO
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'CBO'
068600030822     C                   EVAL      VABCBO=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   ENDIF
069000030822     C* TSP
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'TSP'
069500030822     C                   EVAL      VABTSP=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   ENDIF
069900030822     C* VAS
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'VAS'
070400030822     C                   EVAL      VABVAS=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800030822     C* VCA
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'VCA'
071300030822     C                   EVAL      VABVCA=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   ENDIF
071700070925     C* TIC
071800070925     C                   IF        %subst(
071900070925     C                             %subst(vlrppt:posDaDft+1:
072000070925     C                             posADft-posDaDft-1):1:3)
072100070925     C                             = 'TIC'
072200070925     C                   EVAL      VABTIC=%trim(%subst(
072300070925     C                             %subst(vlrppt:posDaDft+1:
072400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500070925     C                   ENDIF
072600030822     C* GCA
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'GCA'
073100030822     C                   EVAL      VABGCA=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500030822     C* CTM
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'CTM'
074000030822     C                   EVAL      VABCTM=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   ENDIF
074400030822     C* FFD
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'FFD'
074900030822     C                   EVAL      VABFFD=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* VAD
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'VAD'
075800030822     C                   EVAL      VABVAD=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* GMA
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'GMA'
076700030822     C                   EVAL      VABGMA=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* GGA
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'GGA'
077600030822     C                   EVAL      VABGGA=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* GVA
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'GVA'
078500030822     C                   EVAL      VABGVA=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* TC1
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'TC1'
079400030822     C                   EVAL      VABTC1=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* TC2
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'TC2'
080300030822     C                   EVAL      VABTC2=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700040714     C* VATTRC
080800040714     C                   IF        %subst(
080900040714     C                             %subst(vlrppt:posDaDft+1:
081000040714     C                             posADft-posDaDft-1):1:3)
081100040714     C                             = 'TRC'
081200040714     C                   EVAL      VATTRC=%trim(%subst(
081300040714     C                             %subst(vlrppt:posDaDft+1:
081400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500040714     C                   ENDIF
081600030822     C* ...
081700030822     C                   ENDIF
081800030822     C                   ENDDO
081900020204     C*
082000010330     C                   ENDSR
082100010607     C*----------------------------------------------------*
082200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082300010607     C*----------------------------------------------------*
082400010607     C     IMPVAB        BEGSR
082500010607     C*
082600010607     C                   SETOFF                                       3132
082700030515     C*
082800010607     C                   MOVEL     datcor        VABAAS
082900010607     C                   MOVE      datcor        VABMGS
083000010607     C*
083100030715     C**********
083200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083300030715     C**********
083400030715     C                   IF        wGiro = *zeros
083500040113     C*
083600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083900080429     C                             %trim(intcol(5))<>*blanks
084000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084200080429     C                                      %trim(intcol(5))
084300070823     C     ';':CharCSV   XLATE     vindta        vindta
084400040510     C                   ENDIF
084500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084600031201     C     minu:maiu     XLATE     vindta        vindta
084700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084800030715     C                   DOW       posDa <= %len(%trim(vindta))
084900030715     C*
085000030715     C* Gestisco il 1° campo
085100030715     C                   IF        i = 1
085200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085300030715     C                   EVAL      posDa = 1
085400030715     C* Eseguo lo scan x trovare la fine del primo campo
085500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085600030715     C* A questo punto "estraggo" il campo corrente
085700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085800030715     C                                                    (posA-posDa))
085900030715     C* X i campi successivi al 1°
086000030715     C                   ELSE
086100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086200030715     C                   EVAL      posDa = posA + 1
086300030715     C* Eseguo lo scan x trovare la fine del campo corrente
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* Gestisco l'ultimo campo
086600030715     C                   IF        posA = *zeros
086700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C                   LEAVE
087200030715     C                   ELSE
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C                   ENDIF
087700030715     C                   ENDIF
087800030715     C* Incremento il contatore d campo
087900030715     C                   EVAL      i = i +1
088000030715     C                   ENDDO
088100030715     C*
088200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088300030715     C* delimitazione testo
088400030715     C                   EVAL      i = 1
088500030715     C                   DOW       i < %elem(SkSplitFLD)
088600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088800030715     C                   EVAL      i = i + 1
088900030715     C                   ENDDO
089000030715     C                   EVAL      wGiro = 1
089100030715     C                   ELSE
089200030715     C                   EVAL      wGiro = 2
089300031204     C**********
089400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089500031204     C* d separatore campo
089600031204     C**********
089700031204     C                   EVAL      vindta = %trim(vindta)
089800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089900031204     C                   EVAL      vindta = %subst(vindta:2)
090000031204     C                   ENDDO
090100031204     C*
090200031204     C                   Z-ADD     *zeros        lunghInput        4 0
090300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090700031204     C                   ENDDO
090800030715     C**********
090900030715     C* Effettuo lo split del campo dati d input
091000030715     C**********
091100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091200030715     C                   DOW       posDa <= %len(%trim(vindta))
091300030715     C*
091400030715     C* Gestisco il 1° campo
091500030715     C                   IF        i = 1
091600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091700030715     C                   EVAL      posDa = 1
091800030715     C* Eseguo lo scan x trovare la fine del primo campo
091900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092000030715     C* A questo punto "estraggo" il campo corrente
092100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092200030715     C                                                    (posA-posDa))
092300030715     C* X i campi successivi al 1°
092400030715     C                   ELSE
092500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092600030715     C                   EVAL      posDa = posA + 1
092700031204     C* Verifico che nn vi sia il campo nn valorizzato
092800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092900031204     C* Se campo nn valorizzato skippo al prossimo
093000031204     C                   EVAL      SkSplitCSV(i) = *blanks
093100031204     C                   EVAL      posA  = posA + 1
093200031204     C                   ELSE
093300030715     C* Eseguo lo scan x trovare la fine del campo corrente
093400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093500030715     C* Gestisco l'ultimo campo
093600030715     C                   IF        posA = *zeros
093700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093800030715     C* A questo punto "estraggo" il campo corrente
093900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094000030715     C                                                    (posA-posDa))
094100030715     C                   LEAVE
094200030715     C                   ELSE
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   ENDIF
094700031204     C                   ENDIF
094800030715     C                   ENDIF
094900030715     C* Incremento il contatore d campo
095000030715     C                   EVAL      i = i +1
095100030715     C                   ENDDO
095200030715     C*
095300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095400030715     C* delimitazione testo
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitCSV)
095700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095900030715     C                   EVAL      i = i + 1
096000030715     C                   ENDDO
096100030715     C*
096200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096300030715     C                   EVAL      i = 1
096400030715     C                   DOW       i < %elem(SkSplitFLD)
096500030715     C*
096600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096800030715     C                             SkSplitCSV(i) <> *blanks
096900030721     C*** ATB
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097100030721     C                   EVAL      VABATB = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030715     C*** RSD
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097800030715     C* ==
097900030715     C                   ENDIF
098000030721     C*** RD2
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030715     C*** IND
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098600030715     C                   EVAL      VABIND = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** LOD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** PRD
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030721     C*** NZD
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030715     C*** RMA
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** NOT
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** NT2
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** VCA
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** VAS
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** FFD
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030721     C*** GC1
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** GC2
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** TSP
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103500030721     C                   ENDIF
103600040510     C*** CBO
103700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103900040510     C                   ENDIF
104000030721     C*** NAS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** TIC
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GCA
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** XCO
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** CTM
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TCR
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** CTS
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** FTM
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** VAD
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GMA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GGA
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** GVA
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TC1
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** TC2
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** SCL
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** RMO
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** NMO
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*
110900030721     C* Reperisco quindi i campi numerici...
111000040510     C*** NSP
111100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111200040510     C                   EVAL      PiStr=SkSplitCSV(i)
111300040510     C                   EXSR      CHKNUM
111400040510     C                   IF        PiInt=*on
111500040510     C                   Z-ADD     PiVal         VABNSP
111600040510     C                   ELSE
111700040510     C                   SETON                                        31
111800040510     C                   EVAL      VABNSP = *zeros
111900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112000040510     C                             + ' ' + 'VABNSP'
112100040510     C                   ENDIF
112200040510     C                   ENDIF
112300071106     C*** RMN
112400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112500071106     C                   EVAL      PiStr=SkSplitCSV(i)
112600071106     C                   EXSR      CHKNUM
112700071106     C                   IF        PiInt=*on
112800071106     C                   Z-ADD     PiVal         VABRMN
112900071106     C                   ELSE
113000071106     C                   SETON                                        32
113100071106     C                   EVAL      VABRMN = *zeros
113200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113300071106     C                             + ' ' + 'VABRMN'
113400071106     C                   ENDIF
113500071106     C                   ENDIF
113600030721     C*** CAD
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiInt=*on
114100030721     C                   Z-ADD     PiVal         Num5_0
114200030721     C                   MOVEL(p)  Num5_0        VABCAD
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABCAD = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABCAD'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** NCL
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on
115500030721     C                   Z-ADD     PiVal         VABNCL
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      VABNCL = *zeros
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000040122     C                             + ' ' + 'VABNCL'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** PKB
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABPKB
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABPKB = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABPKB'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** CAS
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
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
141700060331     C                   ENDIF
141800060331     C*** VATNOT_E
141900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142100060331     C                   ENDIF
142200070823     C*
142300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
142400080513     C*** NSP/RMA
142500080513     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
142600080513     C                   EVAL      VABRMA=SkSplitCSV(i)
142700080513     C                   EVAL      PiStr=SkSplitCSV(i)
142800080513     C                   EXSR      CHKNUM
142900080513     C                   IF        PiInt=*on
143000080513     C                   Z-ADD     PiVal         VABNSP
143100080513     C                   Z-ADD     PiVal         VATNSP
143200080513     C                   ELSE
143300080513     C                   SETON                                        31
143400080513     C                   EVAL      VABNSP = *zeros
143500080513     C                   EVAL      VATNSP = *zeros
143600080513     C                   EVAL      vinmsg = %trimr(vinmsg)
143700080513     C                             + ' ' + 'VABNSP VATNSP'
143800080513     C                   ENDIF
143900080513     C                   ENDIF
144000080513     C*** NCL
144100080521     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNCL'
144200080513     C                   EVAL      PiStr=SkSplitCSV(i)
144300080513     C                   EXSR      CHKNUM
144400080513     C                   IF        PiInt=*on
144500080527     C                   Z-ADD     PiVal         VABNCL
144600080513     C                   ELSE
144700080513     C                   SETON                                        32
144800080527     C                   EVAL      VABNCL = *zeros
144900080513     C                   EVAL      vinmsg = %trimr(vinmsg)
145000080513     C                             + ' ' + 'VABNCL'
145100080513     C                   ENDIF
145200080513     C                   ENDIF
145300080513     C*** PKB
145400080521     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
145500080513     C                   EVAL      PiStr=SkSplitCSV(i)
145600080513     C                   EXSR      CHKNUM
145700080513     C                   IF        PiNum=*on
145800080527     C                   Z-ADD     PiVal         VABPKB
145900080513     C                   ELSE
146000080513     C                   SETON                                        32
146100080527     C                   EVAL      VABPKB = *zeros
146200080513     C                   EVAL      vinmsg = %trimr(vinmsg)
146300080513     C                             + ' ' + 'VABPKB'
146400080513     C                   ENDIF
146500080513     C                   ENDIF
146600070925     C*
146700071029     C***  <===  -----------------------------------------
146800040510     C*
146900030715     C                   ENDIF
147000030715     C                   EVAL      i = i + 1
147100030715     C                   ENDDO
147200070925     C*
147300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147400070925     C*
147500071029     C***  <===  -----------------------------------------
147600070925     C*
147700070925     C* Considerazioni finali su CBO/CAS
147800080418     C                   IF        wFlgCAS = '1'
147900070925     C                   IF        VABCBO = '1'
148000070925     C                   EVAL      VABCBO = '4'
148100070925     C                   ENDIF
148200070925     C                   IF        VABCBO = '2'
148300070925     C                   EVAL      VABCBO = '6'
148400070925     C                   ENDIF
148500070925     C                   ENDIF
148600070925     C***  <===  ----------------------------
148700040510     C*
148800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148900040510     C                   IF        VABNSP = *zeros
149000040510     C                   SETON                                        31
149100040510     C                   ENDIF
149200040715     C*
149300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149400040715     C                   IF        VABLNP = *zeros
149500040715     C                   SETON                                        31
149600040715     C                   ENDIF
149700040510     C*
149800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149900040510     C                   IF        VABRMN = *zeros
150000040510     C                   Z-ADD     VABNSP        VABRMN
150100040510     C                   ENDIF
150200040510     C*
150300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150400040510     C                   IF        VABRMA = *blanks
150500040714     C                   MOVEL     VABRMN        VABRMA
150600040510     C                   ENDIF
150700040128     C*
150800040510     C* Se provincia nn valorizzata la reperisco
150900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151000040510     C                   IF        VABPRD = *blanks
151100040128     C                   CLEAR                   TISI95DS
151200040128     C                   EVAL      I95TCN = '3'
151300040128     C                   Z-ADD     datcor        I95DAT
151400040128     C                   EVAL      I95NAR = VABNZD
151500040128     C                   EVAL      I95CAP = VABCAD
151600040128     C                   EVAL      I95LOC = VABLOD
151700040128     C                   CALL      'TISI95R'
151800040128     C                   PARM                    TISI95DS
151900040128     C                   EVAL      VABPRD = O95PRV
152000040510     C                   ENDIF
152100040113     C*
152200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
152300040113     C                   IF        VABCAS > *zeros
152400040119     C                   EVAL      VABVCA = 'EUR'
152500040119     C                   IF        VABCBO = '1'
152600040113     C                   EVAL      VABCBO = '4'
152700040119     C                   ENDIF
152800040119     C                   IF        VABCBO = '2'
152900040119     C                   EVAL      VABCBO = '6'
153000040119     C                   ENDIF
153100040113     C                   ENDIF
153200020204     C*
153300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153400020204     C                   EXSR      CHKIMPDIV
153500030715     C                   ENDIF
153600020204     C*
153700010607     C                   ENDSR
153800010607     C*----------------------------------------------------*
153900010601
154000020204
154100020204     C*----------------------------------------------------*
154200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154300020204     C*----------------------------------------------------*
154400020204     C     CHKIMPDIV     BEGSR
154500020204     C*
154600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154700020204     C                   Z-ADD     *zeros        wrkDec            9 9
154800020204     C*
154900020204     C* Come prima cosa effettuo considerazioni sulla divisa
155000020204     C                   IF        vabIAS > *zeros
155100020204     C                   IF        vabVAS <> 'EUR'
155200020204     C                   EVAL      vabVAS =  'ITL'
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C                   IF        vabCAS > *zeros
155700020204     C                   IF        vabVCA <> 'EUR'
155800020204     C                   EVAL      vabVCA =  'ITL'
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C                   IF        vabVMD > *zeros
156300020204     C                   IF        vabVAD <> 'EUR'
156400020204     C                   EVAL      vabVAD =  'ITL'
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156900020204     C                   Z-ADD     vabIAS        wrkDec
157000020204     C                   IF        wrkDec > *zeros
157100020204     C                   IF        vabVAS = 'ITL'
157200020204     C                   EVAL      vabIAS = *zeros
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157700020204     C                   Z-ADD     vabCAS        wrkDec
157800020204     C                   IF        wrkDec > *zeros
157900020204     C                   IF        vabVCA = 'ITL'
158000020204     C                   EVAL      vabCAS = *zeros
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158500020204     C                   Z-ADD     vabVMD        wrkDec
158600020204     C                   IF        wrkDec > *zeros
158700020204     C                   IF        vabVAD = 'ITL'
158800020204     C                   EVAL      vabVMD = *zeros
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C                   ENDSR
159300020204     C***
159400020204
159500010330
159600010330
159700010330     C*----------------------------------------------------*
159800010330     C*  CONTROLLO NUMERICITA' CAMPI
159900010330     C*----------------------------------------------------*
160000010330     C     CHKNUM        BEGSR
160100010330     C*
160200010606     C                   IF        PiDecChr = *blanks
160300030715     C                   EVAL      PiDecChr = CharNUM
160400010606     C                   ENDIF
160500010606     C*
160600010606     C                   CALL(e)   'ISNUMERIC'
160700010330     C                   PARM                    PiStr            30
160800010606     C                   PARM                    PiDecChr          1
160900010330     C                   PARM      *ZEROS        PiVal            30 9
161000010330     C                   PARM      '0'           PiInt             1
161100010330     C                   PARM      '0'           PiNum             1
161200010330     C                   IF        %error
161300010606     C                   EVAL      PiNum=*off
161400010330     C                   ENDIF
161500010330     C*
161600010330     C                   ENDSR
161700010330     C***
161800010330
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
175300080418
175400080418
175500080418
175600080418      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
175700080418     C     exeBon        BEGSR
175800080418     C*
175900080418     C* Imposto i parametri x il *pgm d "bonifica"
176000080418     C                   EVAL      ElencoChar = '1234567890'
176100080418     C                   EVAL      TipoElenco = *blanks
176200080418     C                   EVAL      CharSost   = *blanks
176300080418     C                   EVAL      UpperCase  = *blanks
176400080418     C                   EVAL      ChkNull    = '1'
176500080418     C                   EVAL      CharNull   = *blanks
176600080418     C*
176700080418     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VABRMA
176800080418     C                   EVAL      TxtInOut   = VABRMA
176900080418     C                   EVAL      EsitoBon   = *blanks
177000080418     C*
177100080418     C                   CALL      'XCHKCHAR'
177200080418     C                   PARM                    TxtInOut
177300080418     C                   PARM                    ElencoChar
177400080418     C                   PARM                    TipoElenco
177500080418     C                   PARM                    CharSost
177600080418     C                   PARM                    UpperCase
177700080418     C                   PARM                    ChkNull
177800080418     C                   PARM                    CharNull
177900080418     C                   PARM                    EsitoBon
178000080418     C*
178100080418     C                   ENDSR
178200070326
178300070326
178400010330
178500010330
178600000613     C     *inzsr        BEGSR
178700990910     C*
178800990910     C     *entry        plist
178900990920     C                   parm                    tivlrds
179000990921     C                   parm      wrkesito      esito
179100000724     C                   parm                    prmlit
179200000710     C                   parm                    prmfir
179300010330     C*
179400010330     C* CALCOLA LA DATA CORRENTE
179500010330     C                   time                    wn14             14 0
179600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
179700010330     C                   z-add     wn8           g08dat
179800010330     C                   z-add     *zeros        g08inv
179900010330     C                   movel     '0'           g08err
180000010330     C                   call      'XSRDA8'
180100010330     C                   parm                    wlbda8
180200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
180300000613     C*
180400000613     C                   ENDSR
180500000613     C***
180600990908
180700080429     Otitv1g1p  E            riepilogo         2
180800990915     O                                              'Upload via Internet'
180900990915     O                                           +1 'Traduzione TIVIN00R -'
181000021025     O                                           +1 'FIVABWWR'
181100990915     O                                           +1 'Testate bolle'
181200990915     O          E            riepilogo   2
181300990915     O                       wrkdata
181400990915     O                       wrkora              +1
181500990915     O                       procname            +1
181600990915     O          E            riepilogo   2
181700990915     O                                              'Cliente..................:'
181800010605     O                       VABCCM        z     +1
181900990915     O          E            riepilogo   2
182000990920     O                                              'Riferimento Strategi.....:'
182100990920     O                       vlrhdl              +1
182200990915     O          E            riepilogo   2
182300990915     O                                              'Giusti...................:'
182400040119     O                       §CTROKVB      2   +  1
182500990915     O          E            riepilogo   2
182600990915     O                                              'Sbagliati e corretti.....:'
182700971022     O                       §CTRMO        2   +  1
182800990915     O          E            riepilogo   2
182900990915     O                                              'Sbagliati e scartati.....:'
183000971022     O                       §CTRNO        2   +  1
183100000613
183200080429     Otitv1g1ps E            testdett          2
183300000613     O                                              'Upload via Internet'
183400000613     O                                           +1 'Traduzione TIVIN00R -'
183500021025     O                                           +1 'FIVABWWR'
183600000613     O                                           +1 'Testate bolle'
183700000616     O          E            testdett    3
183800000613     O                                           +2 'N° rec'
183900000613     O                                           +3 'Anteprima contenuto'
184000000616     O          E            rigadett    2
184100000613     O                       rrnum               +2
184200000621     O                       recko               +3
184300000616     O          E            findett     2
184400000613     O                       wrkdata
184500000613     O                       wrkora              +1
184600000613     O                       procname            +1
184700000616     O          E            findett     2
184800000613     O                                              'Cliente..................:'
184900010605     O                       VABCCM        z     +1
185000000616     O          E            findett     2
185100000613     O                                              'Riferimento Strategi.....:'
185200000613     O                       vlrhdl              +1
185300000616     O          E            findett     2
185400000613     O                                              'Giusti...................:'
185500040119     O                       §CTROKVB      2   +  1
185600000616     O          E            findett     2
185700000613     O                                              'Sbagliati e corretti.....:'
185800000613     O                       §CTRMO        2   +  1
185900000616     O          E            findett     2
186000000613     O                                              'Sbagliati e scartati.....:'
186100000613     O                       §CTRNO        2   +  1
186200000613** CMD - COMANDI CL
186300080429OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1G1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
186400080429OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1G1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
186500080429DLTOVR FILE(TITV1G1P TITV1G1PS) LVL(*)
186600010606
186700000613
186800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186900080521VABNSP|VABRMN|VATNOT_E|W_VABNCL|FILL01|W_VABPKB|FILL02|FILL03|VABRSD|VABRD2|
187000080429FILL04|FILL05|VABIND|FILL05|VABCAD|VABLOD|FILL06|FILL07|FILL08|FILL09|
187100080429FILL10|FILL11|FILL12|FILL13|FILL14|FILL15|FILL16|FILL17|FILL18|FILL19|
187200080429FILL20|FILL21|FILL22|FILL23|FILL24|FILL25|FILL26|
187300080429
