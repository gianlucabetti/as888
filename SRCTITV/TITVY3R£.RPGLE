000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400070306     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021025     FFIVABwwr  O    E             DISK    usropn
000800040119     FFIVATwwr  O    E             DISK    usropn
000900070306     Ftitvy3p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200070306     Ftitvy3ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070411     F                                     infsr(*pssr)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040113     D*------------
002100040113     D* SCHIEDE A PROGRAMMA
002200040113     D*------------
002300040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002400010330     D*----------------------------------------------------
002500010330     D* DICHIARAZIOINE VARIABILI DI WRK
002600010330     D*----------------------------------------------------
002700010330     D dscmz         e ds                  inz
002800010330     D psds           sds
002900010330     D  procname         *PROC
003000010330     D tivlrds       e ds                  extname(tivlr00f)
003100040128     D tisi95ds      e ds
003200070306     D tivlrds_rc    e ds                  extname(tivlr00f) prefix(rc_)
003300070306     D fivabds       e ds                  extname(fivab00f)
003400070306     D dosr          e ds                  inz
003500080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
003600080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
003700080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
003800080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003900080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
004000080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
004100080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
004200080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
004300080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
004400080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
004500080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
004600010330     D esito           s              1
004700010330     D prmlit          s             10
004800010330     D prmfir          s             10
004900010330     D wrkesito        s                   like(esito)
005000010330     D wrkdata         s               d
005100010330     D wrkora          s               t
005200010330     D rrnum           s              6  0 INZ(*zeros)
005300010330     D recko           s            150    INZ(*blanks)
005400011113     D depcmd          s            150    INZ(*blanks)
005500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
005600040510     D parccm          s              8    INZ(*blanks)
005700040119     D parmbr          s             10    INZ(*blanks)
005800040119     D paropz          s              1    INZ(*blanks)
005900040119     D chkcall         s              1    INZ(*blanks)
006000070306     D wNSR            s              2    INZ
006100070306     D wfivabds        s                   LIKE(fivabds)
006200030715     D*------------------
006300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006400030715     D*------------------
006500030715     D SkSplitFLD      S             10    DIM(100)
006600030715     D SkSplitCSV      S            256    DIM(100)
006700030715     D CharCSV         S              1
006800030715     D CharTXT         S              1
006900030715     D CharNUM         S              1
007000030715     D posDa           S              3  0 INZ(*zeros)
007100030715     D posA            S              3  0 INZ(*zeros)
007200030715     D i               s              3  0 INZ(1)
007300030715     D wGiro           s              1  0 INZ(*zeros)
007400060331     D wVATNOT_A       s                   LIKE(VATNOT)
007500060331     D wVATNOT_B       s                   LIKE(VATNOT)
007600060331     D wVATNOT_E       s                   LIKE(VATNOT)
007700060331     D wVABNSP         s                   LIKE(VABNSP) INZ
007800030822     D*------------------
007900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008000030822     D*------------------
008100030822     D posDaDft        S              3  0 INZ(*zeros)
008200030822     D posADft         S              3  0 INZ(*zeros)
008300030822     D j               s              3  0 INZ(1)
008400030822     D wGiroDft        s              1  0 INZ(*zeros)
008500010330     D*------------------
008600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008700010330     D*------------------
008800010330     D WLBDA8          DS                  INZ
008900010330     D  G08DAT                 1      8  0
009000010330     D  G08INV                 9     16  0
009100010330     D  G08ERR                17     17
009200010330     D  G08TGI                18     22  0
009300080502     D*------------------
009400080502     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
009500080502     D*------------------
009600080502     D SkCliOSR        s             10    DIM(900)
009700080502     D*
009800080502     D DsCliOSR        ds
009900080502     D  wLnpOSR                       3
010000080502     D  wCliOSR                       7
010100080502     D*
010200080502     D SkLnpNrsOSR     s              5    DIM(900)
010300031201     D*------------------
010400031201     D* Costanti
010500031201     D*------------------
010600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010800010330
010900010330
011000010330
011100990915     C                   time                    wrkdata
011200990915     C                   time                    wrkora
011300000913     C                   reset                   rrnum
011400990921     C                   reset                   esito
011500990921     C                   reset                   wrkesito
011600010601     C*
011700010601     C                   exsr      opeini
011800010605     C                   exsr      rwvab
011900010601     C*
012000010601     C                   seton                                        lr
012100010601
012200010601
012300010601
012400010601     C*--------------------------------------------------------
012500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012600010601     C*--------------------------------------------------------
012700010601     C     PREELA        BEGSR
012800010601     C*
012900010601     C* SE OCCORRE SPEDIRE IN FILIALE
013000010601     C                   if        invfil <> *zeros and
013100010601     C                             flgGiro = '0'
013200010601     C*
013300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013400010601     C                   eval      flgGiro = '1'
013500000724     C*
013600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013800010601     C     invfil        chain     azorg01l
013900000724     C                   if        %found
014000000724     C                   movel(p)  CMD(1)        depcmd
014100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014200000724     C*
014300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000724     C                   PARM                    depcmd
014700000724     C                   PARM                    LENGH
014800000724     C*
014900000724     C                   endif
015000000616     C*
015100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015200000616     C                   movel(p)  CMD(2)        depcmd
015300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015400000616     C*
015500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015600011113     C                   Z-ADD     150           LENGH            15 5
015700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015800000616     C                   PARM                    depcmd
015900000616     C                   PARM                    LENGH
016000000613     C*
016100070306     C                   if        not %open(titvy3ps)
016200070306     C                   open      titvy3ps
016300030715     C                   except    testdett
016400000613     C                   endif
016500010601     C*
016600010601     C                   endif
016700010601     C*
016800010601     C                   ENDSR
016900010601     C***
017000010601
017100010601
017200010601
017300010601
017400010601     C*--------------------------------------------------------
017500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017600010601     C*--------------------------------------------------------
017700010601     C     ENDELA        BEGSR
017800990915     C*
017900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018000040119     C*
018100040119     C* Effettuo la chiamata al CLLE preposto
018200040119     C                   call(e)   'TITVVTC'
018300040510     C                   parm                    parccm
018400040119     C                   parm                    parmbr
018500040119     C                   parm      '2'           paropz
018600050201     C*
018700050201     C* Effettuo lancio TISI95 solo x chiusura
018800050201     C                   CLEAR                   TISI95DS
018900050201     C                   EVAL      I95TLA = 'C'
019000050201     C                   CALL      'TISI95R'
019100050201     C                   PARM                    TISI95DS
019200000613     C*
019300070306     C                   if        %open(titvy3ps)
019400000616     C                   except    findett
019500070306     C                   close     titvy3ps
019600000613     C                   endif
019700000616     C*
019800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019900011113     C                   Z-ADD     150           LENGH            15 5
020000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020100000616     C                   PARM                    CMD(3)
020200000616     C                   PARM                    LENGH
020300070306     C*
020400070306     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
020500070306     C   54              call(e)   'TITV84R'
020600070306     C                   parm      'C'           InTipl
020700070306     C                   parm                    tivlrds_rc
020800070306     C                   parm                    wfivabds
020900000616     C*
021000010601     C                   ENDSR
021100010601     C***
021200010601
021300010601
021400010601
021500000613
021600000613     C*--------------------------------------------------------
021700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021800000613     C*--------------------------------------------------------
021900000621     C     STPR          BEGSR
022000000613     C*
022100070306     C                   if        not %open(titvy3p)
022200070306     C   50              open      titvy3p
022300990915     C                   endif
022400990915     C*
022500030715     C   50              except    riepilogo
022600990915     C*
022700070306     C                   if        %open(titvy3p)
022800070306     C   50              close     titvy3p
022900990914     C                   endif
023000000613     C*
023100000613     C                   ENDSR
023200000613     C***
023300000613
023400010601
023500010601
023600010330     C*--------------------------------------------------------
023700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023800010330     C*--------------------------------------------------------
023900010605     C     RWVAB         BEGSR
024000010330     C*
024100010330     C                   if        not %open(tivin00r)
024200010330     C                   open      tivin00r
024300010330     C                   endif
024400021025     C                   if        not %open(fivabwwr)
024500021025     C                   open      fivabwwr
024600010330     C                   endif
024700040119     C*
024800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024900040119     C                   exsr      prevat
025000040119     C*
025100040119     C                   if        chkcall = '0'
025200040119     C*
025300040119     C                   if        not %open(fivatwwr)
025400040119     C                   open      fivatwwr
025500040119     C                   endif
025600010330     C*
025700040119     C                   clear                   §CTROKVB          7 0
025800040119     C                   clear                   §CTROKVT          7 0
025900040119     C                   clear                   §CTRMO            7 0
026000040119     C                   clear                   §CTRNO            7 0
026100010330     C*
026200030822     C                   do        *HIVAL
026300010330     C*
026400030822     C                   read      tivin00r                               70
026500010618     C*
026600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026700010618     C                   if        vinflg = '1'
026800010618     C                   eval      flgOk = '1'
026900010618     C                   endif
027000040510     C*
027100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027200040510     C                   z-add     *zeros        wLenVINDTA        4 0
027300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027700040510     C                   z-add     1             wFlgVINDTA
027800040510     C                   leave
027900040510     C                   endif
028000040510     C                   enddo
028100010618     C*
028200040510     C                   if        vindta > *blanks AND
028300040510     C                             wFlgVINDTA = 1
028400010330     C                   add       1             rrnum
028500010330     C*
028600010601     C                   if        *in70 = *off and
028700010330     C                             (vinflg = *blanks
028800010330     C                              or vinflg = '0'
028900010330     C                              or vinflg = '2')
029000010330     C*
029100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029200010711     C                   if        vinflg = *blanks or vinflg = '0'
029300010711     C                   clear                   vinmsg
029400010711     C                   endif
029500010601     C*
029600010330     C                   exsr      inzvar
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
031000070306     C*
031100070306     C* Occorre considerare tutte le bolle d questo file come ritiri
031200070306     C* contestuali x cui reperisco il numero serie ORM in tabella 'OSR'
031300070306     C                   EVAL      VABGMA = 'RC'
031400070306     C                   Z-ADD     1             jOSR
031500080502     C                   move(p)   VABLNP        wLnpOSR
031600080502     C                   move(p)   VABCCM        wCliOSR
031700080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
031800070306     C                   IF        %found
031900070306     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
032000070306     C                   EVAL      %subst(rc_vlrppt:4:2) = wNSR
032100070306     C                   ELSE
032200070306     C                   SETON                                        31
032300070306     C                   EVAL      vinmsg = %trimr(vinmsg)
032400070306     C                             + ' ' + 'ORMNRS(tabellaOSR)'
032500070306     C                   ENDIF
032600070306     C*
032700070306     C* Forso che l'orm contestuale venga generato con colli e peso = quelli della bolla originale
032800070307     C                   EVAL      %subst(rc_vlrppt:29:1) = 'S'
032900010601     C*
033000010604     C* Ebbene...
033100010604     C*
033200040119     C  N31              add       1             §CTROKVB
033300040119     C   32              add       1             §CTRMO
033400040119     C   31              add       1             §CTRNO
033500030822     C                   if        wGiro = 2
033600040510     C* Scrivo sempre il VAT
033700040510     C  N31              exsr      wrivat
033800040510     C* Scrivo il VAB solo a rottura d spedizione
033900040119     C                   if        depspe =  *zeros OR
034000040119     C                             depspe <> vabNSP
034100040119     C  N31              exsr      wrivab
034200040119     C  N31              eval      depspe = vabNSP
034300070306     C*
034400070306     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
034500070306     C                   if        *IN31 = *OFF AND
034600070306     C                             vabGMA = 'RC'
034700070306     C*
034800070306     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
034900070306     C                   seton                                        54
035000070306     C*
035100070306     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
035200070306     C                   eval      wfivabds = fivabds
035300070306     C                   call      'TITV84R'
035400070306     C                   parm      *blanks       InTipl            1
035500070306     C                   parm                    tivlrds_rc
035600070306     C                   parm                    wfivabds
035700070306     C                   endif
035800070306     C*
035900040119     C                   endif
036000040119     C                   endif
036100010604     C*
036200010604     C                   if        *in31 = *off and
036300010604     C                             *in32 = *off
036400010604     C                   eval      vinflg = '1'
036500010604     C                   else
036600010604     C                   eval      recko = vindta
036700010604     C                   except    rigadett
036800010604     C                   eval      vinflg = '2'
036900010604     C                   endif
037000010604     C                   endif
037100010601     C*
037200010604     C                   endif
037300010604     C*
037400010330     C                   else
037500010330     C                   eval      vinflg = '1'
037600010330     C                   endif
037700010601     C*
037800010601     C  N70              update    tivin000
037900010330     C*
038000030822     C  N70              enddo
038100040119     C*
038200040119     C                   endif                                                  (endif - chkcall)
038300010601     C*
038400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
038500010601     C                   if        cntNonEl = *zeros or
038600010601     C                             flgMulti = '0'
038700010330     C* Se non ci sono record con errori ...
038800010601     C                   if        §ctrno = 0 and
038900010604     C                             §ctrmo = 0 and
039000010601     C                             flgStato <> '2'
039100010330     C* ... restituisco esito OK.
039200010330     C                   eval      wrkesito = '0'
039300010330     C                   else
039400040119     C                   if        §ctrokvb > 0
039500010330     C                   eval      wrkesito = '1'
039600010330     C                   else
039700010615     C                   if        flgOk = '0'
039800010615     C                   eval      wrkesito = '2'
039900010615     C                   else
040000010615     C                   eval      wrkesito = '6'
040100010615     C                   endif
040200010330     C                   endif
040300010330     C                   endif
040400010601     C                   else
040500010601     C                   eval      wrkesito = '9'
040600010601     C                   endif
040700010330     C*
040800010330     C                   if        %open(tivin00r)
040900010330     C                   close     tivin00r
041000010330     C                   endif
041100021025     C                   if        %open(fivabwwr)
041200021025     C                   close     fivabwwr
041300010330     C                   endif
041400040512     C                   if        %open(fivatwwr)
041500040512     C                   close     fivatwwr
041600040512     C                   endif
041700010601     C*
041800010601     C                   if        vlrpoi <> 999
041900010601     C                   eval      invfil = vlrpoi
042000010601     C                   endif
042100010330     C*
042200040119     C                   if        §ctrokvb > 0
042300010601     C                             and invfil > *zeros
042400010330     C                   exsr      invio
042500010330     C                   endif
042600010601     C*
042700010618     C                   if        flgGiro = '1'
042800010601     C                   exsr      endela
042900010618     C                   endif
043000010330     C*
043100010330     C                   ENDSR
043200010330     C***
043300040119
043400040119
043500040119
043600040119     C*----------------------------------------------------*
043700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
043800040119     C*----------------------------------------------------*
043900040510     C     WRIVAB        BEGSR
044000040119     C*
044100060331     C* Gestisco l'eventuale rottura x numero spedizione
044200060331     C                   if        VABNSP <> wVABNSP
044300070301     C                   if        wVATNOT_E <> *blanks
044400070301     C                   eval      VABCTM = '7Q'
044500070301     C                   endif
044600040119     C                   write     fivab000                                     => scarico il VAB
044700060331     C                   eval      wVABNSP = VABNSP
044800060331     C                   endif
044900040119     C*
045000040119     C                   ENDSR
045100040119
045200040119
045300040119
045400040119     C*----------------------------------------------------*
045500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
045600040119     C*----------------------------------------------------*
045700040510     C     WRIVAT        BEGSR
045800060512     C*
045900060512     C* Inizializzo i campi chiave
046000060512     C                   eval      VATFGS = VABFGS
046100060512     C                   eval      VATAAS = VABAAS
046200060512     C                   eval      VATCCM = VABCCM
046300060512     C                   eval      VATNSP = VABNSP
046400060512     C                   eval      VATLNP = VABLNP
046500040714     C*
046600040714     C* Solo se VATNOT valorizzato scarico il buffer
046700040714     C                   if        VATNOT <> *blanks
046800040119     C*
046900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
047000040510     C                   if        VATTRC = *blanks
047100040510     C                   eval      VATTRC = 'E'
047200040510     C                   endif
047300040510     C                   write     FIVAT000
047400040119     C                   add       1             §CTROKVT
047500040714     C*
047600040714     C                   endif
047700060331     C*
047800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
047900060331     C                   if        wVATNOT_A <> *blanks
048000060331     C                   eval      VATNOT = wVATNOT_A
048100060331     C                   eval      VATTRC = 'A'
048200060331     C                   write     FIVAT000
048300060331     C                   add       1             §CTROKVT
048400060331     C                   endif
048500060331     C*
048600060331     C                   if        wVATNOT_B <> *blanks
048700060331     C                   eval      VATNOT = wVATNOT_B
048800060331     C                   eval      VATTRC = 'B'
048900060331     C                   write     FIVAT000
049000060331     C                   add       1             §CTROKVT
049100060331     C                   endif
049200060331     C*
049300060331     C                   if        wVATNOT_E <> *blanks
049400060331     C                   eval      VATNOT = wVATNOT_E
049500060331     C                   eval      VATTRC = 'E'
049600060331     C                   write     FIVAT000
049700060331     C                   add       1             §CTROKVT
049800060331     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100040119
050200040119
050300040119
050400040119     C*----------------------------------------------------*
050500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050600040119     C*----------------------------------------------------*
050700040119     C     PREVAT        BEGSR
050800040119     C*
050900040119     C* Compongo il nome del membro da dare al FIVATWWR
051000040119     C                   eval      parmbr = vlrhdl
051100040119     C                   movel     'M'           parmbr
051200040510     C                   eval      parccm = vlrksc
051300040119     C                   eval      paropz = '1'
051400040119     C* Effettuo la chiamata al CLLE preposto
051500040119     C                   call(e)   'TITVVTC'
051600040119     C                   parm                    parccm
051700040119     C                   parm                    parmbr
051800040119     C                   parm                    paropz
051900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052000040119     C                   if        %error
052100040119     C                   movel     '1'           chkcall
052200040119     C                   else
052300040119     C                   movel     '0'           chkcall
052400040119     C                   endif
052500040119     C*
052600040119     C                   ENDSR
052700010601
052800010601
052900010601
053000010330     C*----------------------------------------------------*
053100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053200010330     C*----------------------------------------------------*
053300040119     C     INZVAR        BEGSR
053400010330     C*
053500030822     C* Inizializzo variabili di wrk
053600020204     C                   Z-ADD     *zeros        Num5_0            5 0
053700060331     C                   MOVEL     *blanks       wVATNOT_A
053800060331     C                   MOVEL     *blanks       wVATNOT_B
053900060331     C                   MOVEL     *blanks       wVATNOT_E
054000030822     C*
054100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054200030822     C                   CLEAR                   FIVAB000
054300040119     C                   CLEAR                   FIVAT000
054400030822     C                   CLEAR                   SkSplitCSV
054500030822     C                   EVAL      i = 1
054600030822     C                   EVAL      posDa = *zeros
054700030822     C                   EVAL      posA  = *zeros
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
056300030822     C*
056400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
056600030715     C                   SETON                                        50
056700030715     C                   ELSE
056800030715     C                   SETOFF                                       50
056900030715     C                   ENDIF
057000030822     C*
057100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057200030822     C                   EVAL      posDaDft = 1
057300030822     C                   EVAL      posADft  = 0
057400030822     C                   EVAL      wGiroDft = 0
057500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057600030822     C                             posDaDft > 0
057700030822     C*
057800030822     C* Gestisco il 1° giro
057900030822     C                   IF        wGiroDft = 0
058000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058200030822     C* Incremento il contatore dei "giri"
058300030822     C                   EVAL      wGiroDft = 1
058400030822     C                   ELSE
058500030822     C                   EVAL      posDaDft = posADft
058600030822     C                   ENDIF
058700030822     C* Eseguo lo scan x trovare la fine del campo corrente
058800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058900030822     C*
059000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059200030822     C                   IF        posDaDft > 0 AND
059300030822     C                             posADft  > 0
059400030822     C* NCL
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'NCL'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiInt=*on
060400030822     C                   Z-ADD     PiVal         VABNCL
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* CCM
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'CCM'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiInt=*on
061700030822     C                   Z-ADD     PiVal         VABCCM
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* LNP
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'LNP'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiInt=*on
063000030822     C                   Z-ADD     PiVal         VABLNP
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* NRS
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'NRS'
063800030822     C                   EVAL      PiStr=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   EXSR      CHKNUM
064200030822     C                   IF        PiInt=*on
064300030822     C                   Z-ADD     PiVal         VABNRS
064400030822     C                   ENDIF
064500030822     C                   ENDIF
064600030822     C* CTR
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'CTR'
065100030822     C                   EVAL      PiStr=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   EXSR      CHKNUM
065500030822     C                   IF        PiInt=*on
065600030822     C                   Z-ADD     PiVal         VABCTR
065700030822     C                   ENDIF
065800030822     C                   ENDIF
065900030822     C* PKB
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'PKB'
066400030822     C                   EVAL      PiStr=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   EXSR      CHKNUM
066800030822     C                   IF        PiNum=*on
066900030822     C                   Z-ADD     PiVal         VABPKB
067000030822     C                   ENDIF
067100030822     C                   ENDIF
067200030822     C* VLB
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'VLB'
067700030822     C                   EVAL      PiStr=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   EXSR      CHKNUM
068100030822     C                   IF        PiNum=*on
068200030822     C                   Z-ADD     PiVal         VABVLB
068300030822     C                   ENDIF
068400030822     C                   ENDIF
068500030822     C* QFT
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'QFT'
069000030822     C                   EVAL      PiStr=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   EXSR      CHKNUM
069400030822     C                   IF        PiNum=*on
069500030822     C                   Z-ADD     PiVal         VABQFT
069600030822     C                   ENDIF
069700030822     C                   ENDIF
069800030822     C* CBO
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'CBO'
070300030822     C                   EVAL      VABCBO=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* TSP
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'TSP'
071200030822     C                   EVAL      VABTSP=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* VAS
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VAS'
072100030822     C                   EVAL      VABVAS=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* VCA
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'VCA'
073000030822     C                   EVAL      VABVCA=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
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
084600040510     C                             %trim(intcol(3))<>*blanks
084700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084800040113     C                                      %trim(intcol(3))
084900040510     C                   ENDIF
085000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085100031201     C     minu:maiu     XLATE     vindta        vindta
085200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085300030715     C                   DOW       posDa <= %len(%trim(vindta))
085400030715     C*
085500030715     C* Gestisco il 1° campo
085600030715     C                   IF        i = 1
085700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085800030715     C                   EVAL      posDa = 1
085900030715     C* Eseguo lo scan x trovare la fine del primo campo
086000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086100030715     C* A questo punto "estraggo" il campo corrente
086200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086300030715     C                                                    (posA-posDa))
086400030715     C* X i campi successivi al 1°
086500030715     C                   ELSE
086600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086700030715     C                   EVAL      posDa = posA + 1
086800030715     C* Eseguo lo scan x trovare la fine del campo corrente
086900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087000030715     C* Gestisco l'ultimo campo
087100030715     C                   IF        posA = *zeros
087200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C                   LEAVE
087700030715     C                   ELSE
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C                   ENDIF
088200030715     C                   ENDIF
088300030715     C* Incremento il contatore d campo
088400030715     C                   EVAL      i = i +1
088500030715     C                   ENDDO
088600030715     C*
088700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088800030715     C* delimitazione testo
088900030715     C                   EVAL      i = 1
089000030715     C                   DOW       i < %elem(SkSplitFLD)
089100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089300030715     C                   EVAL      i = i + 1
089400030715     C                   ENDDO
089500030715     C                   EVAL      wGiro = 1
089600030715     C                   ELSE
089700030715     C                   EVAL      wGiro = 2
089800031204     C**********
089900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090000031204     C* d separatore campo
090100031204     C**********
090200031204     C                   EVAL      vindta = %trim(vindta)
090300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090400031204     C                   EVAL      vindta = %subst(vindta:2)
090500031204     C                   ENDDO
090600031204     C*
090700031204     C                   Z-ADD     *zeros        lunghInput        4 0
090800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200031204     C                   ENDDO
091300030715     C**********
091400030715     C* Effettuo lo split del campo dati d input
091500030715     C**********
091600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091700030715     C                   DOW       posDa <= %len(%trim(vindta))
091800030715     C*
091900030715     C* Gestisco il 1° campo
092000030715     C                   IF        i = 1
092100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092200030715     C                   EVAL      posDa = 1
092300030715     C* Eseguo lo scan x trovare la fine del primo campo
092400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C* X i campi successivi al 1°
092900030715     C                   ELSE
093000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093100030715     C                   EVAL      posDa = posA + 1
093200031204     C* Verifico che nn vi sia il campo nn valorizzato
093300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093400031204     C* Se campo nn valorizzato skippo al prossimo
093500031204     C                   EVAL      SkSplitCSV(i) = *blanks
093600031204     C                   EVAL      posA  = posA + 1
093700031204     C                   ELSE
093800030715     C* Eseguo lo scan x trovare la fine del campo corrente
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* Gestisco l'ultimo campo
094100030715     C                   IF        posA = *zeros
094200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   LEAVE
094700030715     C                   ELSE
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C                   ENDIF
095200031204     C                   ENDIF
095300030715     C                   ENDIF
095400030715     C* Incremento il contatore d campo
095500030715     C                   EVAL      i = i +1
095600030715     C                   ENDDO
095700030715     C*
095800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095900030715     C* delimitazione testo
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitCSV)
096200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096400030715     C                   EVAL      i = i + 1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096800030715     C                   EVAL      i = 1
096900030715     C                   DOW       i < %elem(SkSplitFLD)
097000030715     C*
097100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097300030715     C                             SkSplitCSV(i) <> *blanks
097400030721     C*** ATB
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097600030721     C                   EVAL      VABATB = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** RSD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098300030715     C* ==
098400030715     C                   ENDIF
098500030721     C*** RD2
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030715     C*** IND
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099100030715     C                   EVAL      VABIND = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** LOD
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** PRD
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030721     C*** NZD
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030715     C*** RMA
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** NOT
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** NT2
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** VCA
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** VAS
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** FFD
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030721     C*** GC1
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GC2
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** TSP
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104000030721     C                   ENDIF
104100040510     C*** CBO
104200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104400040510     C                   ENDIF
104500030721     C*** NAS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TIC
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GCA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** XCO
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** CTM
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** TCR
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** CTS
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** FTM
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** VAD
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GMA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** GGA
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GVA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** TC1
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TC2
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** SCL
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** RMO
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** NMO
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*
111400030721     C* Reperisco quindi i campi numerici...
111500040510     C*** NSP
111600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111700040510     C                   EVAL      PiStr=SkSplitCSV(i)
111800040510     C                   EXSR      CHKNUM
111900040510     C                   IF        PiInt=*on
112000040510     C                   Z-ADD     PiVal         VABNSP
112100040510     C                   ELSE
112200040510     C                   SETON                                        31
112300040510     C                   EVAL      VABNSP = *zeros
112400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040510     C                             + ' ' + 'VABNSP'
112600040510     C                   ENDIF
112700040510     C                   ENDIF
112800070306     C*** RMN
112900070306     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113000070306     C                   EVAL      PiStr=SkSplitCSV(i)
113100070306     C                   EXSR      CHKNUM
113200070306     C                   IF        PiInt=*on
113300070306     C                   Z-ADD     PiVal         VABRMN
113400070306     C                   ELSE
113500070306     C                   SETON                                        31
113600070306     C                   EVAL      VABRMN = *zeros
113700070306     C                   EVAL      vinmsg = %trimr(vinmsg)
113800070306     C                             + ' ' + 'VABRMN'
113900070306     C                   ENDIF
114000070306     C                   ENDIF
114100030721     C*** CAD
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiInt=*on
114600030721     C                   Z-ADD     PiVal         Num5_0
114700030721     C                   MOVEL(p)  Num5_0        VABCAD
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABCAD = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABCAD'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** NCL
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on
116000030721     C                   Z-ADD     PiVal         VABNCL
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABNCL = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500040122     C                             + ' ' + 'VABNCL'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** PKB
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300030721     C                   Z-ADD     PiVal         VABPKB
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABPKB = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABPKB'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CAS
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABCAS
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABCAS = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABCAS'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** CCM
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on      AND
119900030721     C                             PiVal<=9999999 AND
120000030721     C                             PiVal>*zeros
120100030721     C                   Z-ADD     PiVal         VABCCM
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABCCM = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600040510     C                             + ' ' + 'VABCCM'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** LNP
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on    AND
121400030721     C                             PiVal<=999   AND
121500030721     C                             PiVal>*zeros
121600030721     C                   Z-ADD     PiVal         VABLNP
121700030721     C                   ELSE
121800040715     C                   SETON                                        32
121900030721     C                   EVAL      VABLNP = *zeros
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100040510     C                             + ' ' + 'VABLNP'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** NRS
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on AND
122900030721     C                             PiVal<=99
123000030721     C                   Z-ADD     PiVal         VABNRS
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABNRS'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** LNA
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on    AND
124200030721     C                             PiVal<=999   AND
124300030721     C                             PiVal>*zeros
124400030721     C                   Z-ADD     PiVal         VABLNA
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      VABLNA = *zeros
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABLNA'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** CTR
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on  AND
125700030721     C                             PiVal<=999
125800030721     C                   Z-ADD     PiVal         VABCTR
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABCTR'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** IAS
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiNum=*on
127000030721     C                   Z-ADD     PiVal         VABIAS
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABIAS'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** VLB
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiNum=*on
128200030721     C                   Z-ADD     PiVal         VABVLB
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABVLB'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** QFT
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABQFT
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABQFT'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NCD
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999
130700030721     C                   Z-ADD     PiVal         VABNCD
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNCD'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** NCA
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on      AND
131900030721     C                             PiVal<=9999999
132000030721     C                   Z-ADD     PiVal         VABNCA
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABNCA'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** ZNC
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on AND
133200030721     C                             PiVal<=99
133300030721     C                   Z-ADD     PiVal         VABZNC
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABZNC'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** DCR
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on       AND
134500030721     C                             PiVal<=99999999
134600030721     C                   Z-ADD     PiVal         VABDCR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABDCR'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** HCR
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on   AND
135800030721     C                             PiVal<=9999
135900030721     C                   Z-ADD     PiVal         VABHCR
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABHCR'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** VMD
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiNum=*on
137100030721     C                   Z-ADD     PiVal         VABVMD
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABVMD'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** ANT
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on        AND
138300030721     C                             PiVal<=999999999
138400030721     C                   Z-ADD     PiVal         VABANT
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138800030721     C                             + ' ' + 'VABANT'
138900030721     C                   ENDIF
139000030721     C                   ENDIF
139100030721     C*** CMO
139200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on
139600030721     C                   Z-ADD     PiVal         Num5_0
139700030721     C                   MOVEL(p)  Num5_0        VABCMO
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      VABCMO = *zeros
140100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140200030721     C                             + ' ' + 'VABCMO'
140300030721     C                   ENDIF
140400030721     C                   ENDIF
140500040510     C*
140600040510     C* Reperisco i campi del FIVAT
140700040510     C*** VATNOT
140800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141000040510     C                   ENDIF
141100040510     C*** VATTRC
141200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141400040510     C                   ENDIF
141500060331     C*** VATNOT_A
141600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141800060331     C                   ENDIF
141900060331     C*** VATNOT_B
142000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142200060331     C                   ENDIF
142300060331     C*** VATNOT_E
142400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142600060331     C                   ENDIF
142700040510     C*
142800030715     C                   ENDIF
142900030715     C                   EVAL      i = i + 1
143000030715     C                   ENDDO
143100040510     C*
143200070306     C* Se NSP e RMN nn valorizzato considero errore bloccante in traduzione
143300070306     C                   IF        VABNSP = *zeros AND VABRMN = *zeros
143400040510     C                   SETON                                        31
143500040510     C                   ENDIF
143600070306     C*
143700070306     C* Se NSP nn valorizzato ma RMN valorizzato => considero RMN come NSP
143800070306     C                   IF        VABNSP = *zeros AND VABRMN <> *zeros
143900070306     C                   Z-ADD     VABRMN        VABNSP
144000070306     C                   ENDIF
144100040715     C*
144200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144300040715     C                   IF        VABLNP = *zeros
144400040715     C                   SETON                                        31
144500040715     C                   ENDIF
144600040510     C*
144700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
144800040510     C                   IF        VABRMN = *zeros
144900040510     C                   Z-ADD     VABNSP        VABRMN
145000040510     C                   ENDIF
145100040510     C*
145200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145300040510     C                   IF        VABRMA = *blanks
145400040714     C                   MOVEL     VABRMN        VABRMA
145500040510     C                   ENDIF
145600040128     C*
145700040510     C* Se provincia nn valorizzata la reperisco
145800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
145900040510     C                   IF        VABPRD = *blanks
146000040128     C                   CLEAR                   TISI95DS
146100040128     C                   EVAL      I95TCN = '3'
146200040128     C                   Z-ADD     datcor        I95DAT
146300040128     C                   EVAL      I95NAR = VABNZD
146400040128     C                   EVAL      I95CAP = VABCAD
146500040128     C                   EVAL      I95LOC = VABLOD
146600040128     C                   CALL      'TISI95R'
146700040128     C                   PARM                    TISI95DS
146800040128     C                   EVAL      VABPRD = O95PRV
146900040510     C                   ENDIF
147000040113     C*
147100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
147200040113     C                   IF        VABCAS > *zeros
147300040119     C                   EVAL      VABVCA = 'EUR'
147400040119     C                   IF        VABCBO = '1'
147500040113     C                   EVAL      VABCBO = '4'
147600040119     C                   ENDIF
147700040119     C                   IF        VABCBO = '2'
147800040119     C                   EVAL      VABCBO = '6'
147900040119     C                   ENDIF
148000040113     C                   ENDIF
148100020204     C*
148200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148300020204     C                   EXSR      CHKIMPDIV
148400030715     C                   ENDIF
148500020204     C*
148600010607     C                   ENDSR
148700010607     C*----------------------------------------------------*
148800010601
148900020204
149000020204     C*----------------------------------------------------*
149100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149200020204     C*----------------------------------------------------*
149300020204     C     CHKIMPDIV     BEGSR
149400020204     C*
149500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149600020204     C                   Z-ADD     *zeros        wrkDec            9 9
149700020204     C*
149800020204     C* Come prima cosa effettuo considerazioni sulla divisa
149900020204     C                   IF        vabIAS > *zeros
150000020204     C                   IF        vabVAS <> 'EUR'
150100020204     C                   EVAL      vabVAS =  'ITL'
150200020204     C                   ENDIF
150300020204     C                   ENDIF
150400020204     C*
150500020204     C                   IF        vabCAS > *zeros
150600020204     C                   IF        vabVCA <> 'EUR'
150700020204     C                   EVAL      vabVCA =  'ITL'
150800020204     C                   ENDIF
150900020204     C                   ENDIF
151000020204     C*
151100020204     C                   IF        vabVMD > *zeros
151200020204     C                   IF        vabVAD <> 'EUR'
151300020204     C                   EVAL      vabVAD =  'ITL'
151400020204     C                   ENDIF
151500020204     C                   ENDIF
151600020204     C*
151700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151800020204     C                   Z-ADD     vabIAS        wrkDec
151900020204     C                   IF        wrkDec > *zeros
152000020204     C                   IF        vabVAS = 'ITL'
152100020204     C                   EVAL      vabIAS = *zeros
152200020204     C                   ENDIF
152300020204     C                   ENDIF
152400020204     C*
152500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152600020204     C                   Z-ADD     vabCAS        wrkDec
152700020204     C                   IF        wrkDec > *zeros
152800020204     C                   IF        vabVCA = 'ITL'
152900020204     C                   EVAL      vabCAS = *zeros
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153400020204     C                   Z-ADD     vabVMD        wrkDec
153500020204     C                   IF        wrkDec > *zeros
153600020204     C                   IF        vabVAD = 'ITL'
153700020204     C                   EVAL      vabVMD = *zeros
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C                   ENDSR
154200020204     C***
154300020204
154400010330
154500010330
154600010330     C*----------------------------------------------------*
154700010330     C*  CONTROLLO NUMERICITA' CAMPI
154800010330     C*----------------------------------------------------*
154900010330     C     CHKNUM        BEGSR
155000010330     C*
155100010606     C                   IF        PiDecChr = *blanks
155200030715     C                   EVAL      PiDecChr = CharNUM
155300010606     C                   ENDIF
155400010606     C*
155500010606     C                   CALL(e)   'ISNUMERIC'
155600010330     C                   PARM                    PiStr            30
155700010606     C                   PARM                    PiDecChr          1
155800010330     C                   PARM      *ZEROS        PiVal            30 9
155900010330     C                   PARM      '0'           PiInt             1
156000010330     C                   PARM      '0'           PiNum             1
156100010330     C                   IF        %error
156200010606     C                   EVAL      PiNum=*off
156300010330     C                   ENDIF
156400010330     C*
156500010330     C                   ENDSR
156600010330     C***
156700010330
156800010601
156900010601
157000010601
157100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157200010601     C     repfil        BEGSR
157300010601     C*
157400010601     C                   if        invfil = *zeros and
157500010601     C                             depfil > *zeros and
157600010629     C                             (vinflg = *blanks or
157700010629     C                              vinflg = *zeros)
157800010601     C
157900010601     C                   eval      invfil = depfil
158000010601     C                   endif
158100010601     C*
158200010601     C                   if        depfil <> invfil and
158300010601     C                             invfil > *zeros
158400010601     C                   eval      flgMulti = '1'
158500010601     C                   if        vinflg = *blanks
158600010601     C                   add       1             cntNonEl
158700010601     C                   endif
158800010601     C                   endif
158900010601     C*
159000010601     C                   if        vinflg = '2'
159100010601     C                   eval      flgStato = '2'
159200010601     C                   endif
159300010601     C*
159400010601     C                   ENDSR
159500010601     C***
159600010601
159700010601
159800010601
159900010330
160000040119      /TITLE Invio dei dati al punto operativo.
160100040119     C     invio         BEGSR
160200040119     C*
160300040119     C* 1° invio FIVAT
160400040119     C                   reset                   dscmz
160500040119     C                   move      vlrpoi        cmzdst
160600040119     C                   eval      cmzfld = 'FIVATWWR'
160700040119     C                   eval      cmzmbd = vlrhdl
160800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160900040119     C***                if        prmfir = *blanks
161000040119     C                   eval      cmzfla = 'FIVAT00F'
161100040119     C                   eval      cmzmba = 'FIVAT00F'
161200040119     C***                else
161300040119     C***                eval      cmzfla = prmfir
161400040119     C***                eval      cmzmba = prmfir
161500040119     C***                endif
161600040119     C                   eval      cmznrr = *zeros
161700040119     C                   move      §ctrokvt      cmznrr
161800040119     C                   eval      cmzlba = vlrfl1
161900040119     C                   call(e)   'TIS711C'
162000040119     C                   parm                    dscmz
162100040119     C                   parm      *blanks       esito
162200040119     C                   if        %error
162300040119     C                             or cmzerr = '1'
162400040119     C                             or esito  = '1'
162500040119     C                   eval      wrkesito = '3'
162600040119     C                   else
162700040119     C*
162800040119     C* 2° invio FIVAB
162900040119     C                   reset                   dscmz
163000040119     C                   move      vlrpoi        cmzdst
163100040119     C                   eval      cmzfld = vlrfou
163200040119     C                   eval      cmzmbd = vlrhdl
163300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163400040119     C***                if        prmfir = *blanks
163500040119     C                   eval      cmzfla = 'FIVAB00F'
163600040119     C                   eval      cmzmba = 'FIVAB00F'
163700040119     C***                else
163800040119     C***                eval      cmzfla = prmfir
163900040119     C***                eval      cmzmba = prmfir
164000040119     C***                endif
164100040119     C                   eval      cmznrr = *zeros
164200040119     C                   move      §ctrokvb      cmznrr
164300040119     C                   eval      cmzlba = vlrfl1
164400040119     C                   call(e)   'TIS711C'
164500040119     C                   parm                    dscmz
164600040119     C                   parm      *blanks       esito
164700040119     C                   if        %error
164800040119     C                             or cmzerr = '1'
164900040119     C                             or esito  = '1'
165000040119     C                   eval      wrkesito = '3'
165100040119     C                   endif
165200040119     C                   endif
165300040119     C*
165400040119     C                   ENDSR
165500040119     C***
165600010601
165700010601
165800010601
165900010601
166000010601      /TITLE Invio dei dati al punto operativo.
166100010601     C     opeini        BEGSR
166200010601     C*
166300010601     C* Inizializzo flag e contatori operativi
166400010601     C                   movel     '0'           flgGiro           1
166500010601     C                   movel     '0'           flgMulti          1
166600010601     C                   movel     '1'           flgStato          1
166700010615     C                   movel     '0'           flgOk             1
166800010601     C                   z-add     *zeros        cntNonEl         10 0
166900010601     C                   z-add     *zeros        depfil            3 0
167000010601     C                   z-add     *zeros        invfil            3 0
167100010601     C*
167200010601     C                   ENDSR
167300010601     C***
167400010601
167500070306
167600070306
167700080502     C*--------------------------------------------------------
167800080502     C* CARTAB - CARICAMENTO TABELLE                          *
167900080502     C*--------------------------------------------------------
168000080502     C     CARTAB        BEGSR
168100080502     C*
168200080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
168300080502     C                   z-add     *zeros        jOSR              3 0
168400080502     C     'OSR'         setll     tntbe01l
168500080502     C                   if        %equal(tntbe01l)
168600080502     C     'OSR'         reade     tntbe01l
168700080502     C                   dow       not %eof(tntbe01l)
168800080502     C                   eval      DOSR = tbeUNI
168900080502     c*
169000080502     c* Verifica e forzatura x nuovi campi aggiunti
169100080502     c                   if        aD§OSRcl6 = *blanks
169200080502     c                   eval      D§OSRcl6 = *zeros
169300080502     c                   endif
169400080502     c                   if        aD§OSRcl7 = *blanks
169500080502     c                   eval      D§OSRcl7 = *zeros
169600080502     c                   endif
169700080502     c                   if        aD§OSRcl8 = *blanks
169800080502     c                   eval      D§OSRcl8 = *zeros
169900080502     c                   endif
170000080502     c                   if        aD§OSRcl9 = *blanks
170100080502     c                   eval      D§OSRcl9 = *zeros
170200080502     c                   endif
170300080502     c                   if        aD§OSRcl10 = *blanks
170400080502     c                   eval      D§OSRcl10 = *zeros
170500080502     c                   endif
170600080502     c                   if        aD§OSRcl11 = *blanks
170700080502     c                   eval      D§OSRcl11 = *zeros
170800080502     c                   endif
170900080502     c                   if        aD§OSRcl12 = *blanks
171000080502     c                   eval      D§OSRcl12 = *zeros
171100080502     c                   endif
171200080502     c                   if        aD§OSRcl13 = *blanks
171300080502     c                   eval      D§OSRcl13 = *zeros
171400080502     c                   endif
171500080502     c                   if        aD§OSRcl14 = *blanks
171600080502     c                   eval      D§OSRcl14 = *zeros
171700080502     c                   endif
171800080502     c                   if        aD§OSRcl15 = *blanks
171900080502     c                   eval      D§OSRcl15 = *zeros
172000080502     c                   endif
172100080502     c                   if        aD§OSRcl16 = *blanks
172200080502     c                   eval      D§OSRcl16 = *zeros
172300080502     c                   endif
172400080502     c*
172500080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
172600080502     C                   if        D§OSRCLI <> *zeros
172700080502     C                   add       1             jOSR
172800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
172900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
173000080502     C                   movel     D§OSRCLI      wCliOSR
173100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
173200080502     C                   endif
173300080502     C*
173400080502     C                   if        D§OSRCL2 <> *zeros
173500080502     C                   add       1             jOSR
173600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
173700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
173800080502     C                   movel     D§OSRCL2      wCliOSR
173900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
174000080502     C                   endif
174100080502     C*
174200080502     C                   if        D§OSRCL3 <> *zeros
174300080502     C                   add       1             jOSR
174400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
174500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
174600080502     C                   movel     D§OSRCL3      wCliOSR
174700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
174800080502     C                   endif
174900080502     C*
175000080502     C                   if        D§OSRCL4 <> *zeros
175100080502     C                   add       1             jOSR
175200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
175300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
175400080502     C                   movel     D§OSRCL4      wCliOSR
175500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
175600080502     C                   endif
175700080502     C*
175800080502     C                   if        D§OSRCL5 <> *zeros
175900080502     C                   add       1             jOSR
176000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
176100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
176200080502     C                   movel     D§OSRCL5      wCliOSR
176300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
176400080502     C                   endif
176500080502     C*
176600080502     C                   if        D§OSRCL6 <> *zeros
176700080502     C                   add       1             jOSR
176800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
176900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
177000080502     C                   movel     D§OSRCL6      wCliOSR
177100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
177200080502     C                   endif
177300080502     C*
177400080502     C                   if        D§OSRCL7 <> *zeros
177500080502     C                   add       1             jOSR
177600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
177700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
177800080502     C                   movel     D§OSRCL7      wCliOSR
177900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
178000080502     C                   endif
178100080502     C*
178200080502     C                   if        D§OSRCL8 <> *zeros
178300080502     C                   add       1             jOSR
178400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
178500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
178600080502     C                   movel     D§OSRCL8      wCliOSR
178700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
178800080502     C                   endif
178900080502     C*
179000080502     C                   if        D§OSRCL9 <> *zeros
179100080502     C                   add       1             jOSR
179200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
179300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
179400080502     C                   movel     D§OSRCL9      wCliOSR
179500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
179600080502     C                   endif
179700080502     C*
179800080502     C                   if        D§OSRCL10 <> *zeros
179900080502     C                   add       1             jOSR
180000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
180100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
180200080502     C                   movel     D§OSRCL10     wCliOSR
180300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
180400080502     C                   endif
180500080502     C*
180600080502     C                   if        D§OSRCL11 <> *zeros
180700080502     C                   add       1             jOSR
180800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
180900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
181000080502     C                   movel     D§OSRCL11     wCliOSR
181100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
181200080502     C                   endif
181300080502     C*
181400080502     C                   if        D§OSRCL12 <> *zeros
181500080502     C                   add       1             jOSR
181600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
181700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
181800080502     C                   movel     D§OSRCL12     wCliOSR
181900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
182000080502     C                   endif
182100080502     C*
182200080502     C                   if        D§OSRCL13 <> *zeros
182300080502     C                   add       1             jOSR
182400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
182500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
182600080502     C                   movel     D§OSRCL13     wCliOSR
182700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
182800080502     C                   endif
182900080502     C*
183000080502     C                   if        D§OSRCL14 <> *zeros
183100080502     C                   add       1             jOSR
183200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
183300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
183400080502     C                   movel     D§OSRCL14     wCliOSR
183500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
183600080502     C                   endif
183700080502     C*
183800080502     C                   if        D§OSRCL15 <> *zeros
183900080502     C                   add       1             jOSR
184000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
184100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
184200080502     C                   movel     D§OSRCL15     wCliOSR
184300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
184400080502     C                   endif
184500080502     C*
184600080502     C                   if        D§OSRCL16 <> *zeros
184700080502     C                   add       1             jOSR
184800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
184900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
185000080502     C                   movel     D§OSRCL16     wCliOSR
185100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
185200080502     C                   endif
185300080502     C*
185400080502     C     'OSR'         reade     tntbe01l
185500080502     C                   enddo
185600080502     C                   endif
185700080502     C*
185800080502     C                   ENDSR
185900080502     C***
186000070411
186100080502
186200080502
186300070411     C     *pssr         BEGSR
186400070411     C*
186500070411     C                   if        %open(tivin00r)
186600070411     C                   close     tivin00r
186700070411     C                   endif
186800070411     C                   if        %open(fivabwwr)
186900070411     C                   close     fivabwwr
187000070411     C                   endif
187100070411     C                   if        %open(fivatwwr)
187200070411     C                   close     fivatwwr
187300070411     C                   endif
187400070411     C*
187500070411     C* Effettuo la chiamata al CLLE preposto
187600070411     C                   call(e)   'TITVVTC'
187700070411     C                   parm                    parccm
187800070411     C                   parm                    parmbr
187900070411     C                   parm      '2'           paropz
188000070411     C*
188100070411     C                   eval      wrkesito = '2'
188200070411     C*
188300070411     C                   seton                                        LR
188400070411     C*
188500070411     C                   ENDSR     '*CANCL'
188600070411     C***
188700070411
188800010330
188900010330
189000000613     C     *inzsr        BEGSR
189100990910     C*
189200990910     C     *entry        plist
189300990920     C                   parm                    tivlrds
189400990921     C                   parm      wrkesito      esito
189500000724     C                   parm                    prmlit
189600000710     C                   parm                    prmfir
189700090622     C*
189800090622     C                   eval      tivlrds_rc = tivlrds
189900070306     C*
190000070306     C* CARICO LE TABELLE OCCORRENTI
190100070306     C                   exsr      CARTAB
190200010330     C*
190300010330     C* CALCOLA LA DATA CORRENTE
190400010330     C                   time                    wn14             14 0
190500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
190600010330     C                   z-add     wn8           g08dat
190700010330     C                   z-add     *zeros        g08inv
190800010330     C                   movel     '0'           g08err
190900010330     C                   call      'XSRDA8'
191000010330     C                   parm                    wlbda8
191100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
191200000613     C*
191300000613     C                   ENDSR
191400000613     C***
191500990908
191600070306     Otitvy3p   E            riepilogo         2
191700990915     O                                              'Upload via Internet'
191800990915     O                                           +1 'Traduzione TIVIN00R -'
191900021025     O                                           +1 'FIVABWWR'
192000990915     O                                           +1 'Testate bolle'
192100990915     O          E            riepilogo   2
192200990915     O                       wrkdata
192300990915     O                       wrkora              +1
192400990915     O                       procname            +1
192500990915     O          E            riepilogo   2
192600990915     O                                              'Cliente..................:'
192700010605     O                       VABCCM        z     +1
192800990915     O          E            riepilogo   2
192900990920     O                                              'Riferimento Strategi.....:'
193000990920     O                       vlrhdl              +1
193100990915     O          E            riepilogo   2
193200990915     O                                              'Giusti...................:'
193300040119     O                       §CTROKVB      2   +  1
193400990915     O          E            riepilogo   2
193500990915     O                                              'Sbagliati e corretti.....:'
193600971022     O                       §CTRMO        2   +  1
193700990915     O          E            riepilogo   2
193800990915     O                                              'Sbagliati e scartati.....:'
193900971022     O                       §CTRNO        2   +  1
194000000613
194100070306     Otitvy3ps  E            testdett          2
194200000613     O                                              'Upload via Internet'
194300000613     O                                           +1 'Traduzione TIVIN00R -'
194400021025     O                                           +1 'FIVABWWR'
194500000613     O                                           +1 'Testate bolle'
194600000616     O          E            testdett    3
194700000613     O                                           +2 'N° rec'
194800000613     O                                           +3 'Anteprima contenuto'
194900000616     O          E            rigadett    2
195000000613     O                       rrnum               +2
195100000621     O                       recko               +3
195200000616     O          E            findett     2
195300000613     O                       wrkdata
195400000613     O                       wrkora              +1
195500000613     O                       procname            +1
195600000616     O          E            findett     2
195700000613     O                                              'Cliente..................:'
195800010605     O                       VABCCM        z     +1
195900000616     O          E            findett     2
196000000613     O                                              'Riferimento Strategi.....:'
196100000613     O                       vlrhdl              +1
196200000616     O          E            findett     2
196300000613     O                                              'Giusti...................:'
196400040119     O                       §CTROKVB      2   +  1
196500000616     O          E            findett     2
196600000613     O                                              'Sbagliati e corretti.....:'
196700000613     O                       §CTRMO        2   +  1
196800000616     O          E            findett     2
196900000613     O                                              'Sbagliati e scartati.....:'
197000000613     O                       §CTRNO        2   +  1
197100000613** CMD - COMANDI CL
197200070306OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
197300070306OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
197400070306DLTOVR FILE(TITVY3P TITVY3PS) LVL(*)
197500010606
197600000613
197700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
197800040510
197900040122
198000040113
