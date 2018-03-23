000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040122     Ftitv92p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100040122     Ftitv92ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070329     F                                     infsr(*pssr)
001400080506     Ftntbe01l  if   e           k disk
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
002800080506     D fivabds       e ds                  extname(fivab00f)
002900080506     D wtivlrds      e ds                  extname(tivlr00f) prefix(w_)
003000080506     D dosr          e ds                  inz
003100080506     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
003200080506     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
003300080506     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
003400080506     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003500080506     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
003600080506     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
003700080506     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
003800080506     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
003900080506     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
004000080506     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
004100080506     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
004200010330     D psds           sds
004300010330     D  procname         *PROC
004400010330     D tivlrds       e ds                  extname(tivlr00f)
004500040128     D tisi95ds      e ds
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
006000080506     D wCTM            s                   LIKE(vabCTM)
006100080506     D wNSR            s              2    INZ
006200080506     D wCRC            s             10    INZ
006300080506     D wKSC            s              7    INZ
006400080506     D wCTR            s              3    INZ
006500080506     D wfivabds        s                   LIKE(fivabds)
006600080506     D*------------------
006700080506     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
006800080506     D*------------------
006900080506     D SkCliOSR        s             10    DIM(900)
007000080506     D*
007100080506     D DsCliOSR        ds
007200080506     D  wLnpOSR                       3
007300080506     D  wCliOSR                       7
007400080506     D*
007500080506     D SkLnpNrsOSR     s              5    DIM(900)
007600030715     D*------------------
007700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007800030715     D*------------------
007900030715     D SkSplitFLD      S             10    DIM(100)
008000030715     D SkSplitCSV      S            256    DIM(100)
008100030715     D CharCSV         S              1
008200030715     D CharTXT         S              1
008300030715     D CharNUM         S              1
008400090403     D CharSOS         S              1
008500030715     D posDa           S              3  0 INZ(*zeros)
008600030715     D posA            S              3  0 INZ(*zeros)
008700030715     D i               s              3  0 INZ(1)
008800030715     D wGiro           s              1  0 INZ(*zeros)
008900060331     D wVATNOT_A       s                   LIKE(VATNOT)
009000060331     D wVATNOT_B       s                   LIKE(VATNOT)
009100060331     D wVATNOT_E       s                   LIKE(VATNOT)
009200110912     D wVATNOT_I       s                   LIKE(VATNOT)
009300110912     D wVATNOT_J       s                   LIKE(VATNOT)
009400110912     D wVATNOT_4       s                   LIKE(VATNOT)
009500131018     D wVATNOT_S       s                   LIKE(VATNOT)
009600141128     D wVATNOT_Y       s                   LIKE(VATNOT)
009700060331     D wVABNSP         s                   LIKE(VABNSP) INZ
009800030822     D*------------------
009900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
010000030822     D*------------------
010100030822     D posDaDft        S              3  0 INZ(*zeros)
010200030822     D posADft         S              3  0 INZ(*zeros)
010300030822     D j               s              3  0 INZ(1)
010400030822     D wGiroDft        s              1  0 INZ(*zeros)
010500080506     D*------------------
010600080506     D* DS REPERIMENTO NUMERATORE
010700080506     D*------------------
010800080506     D trul33ds      e ds                  inz
010900080506     D*------------------
011000080506     D* DS ARCHITETTURA
011100080506     D*------------------
011200080506     D kpjba         e ds                  inz
011300010330     D*------------------
011400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
011500010330     D*------------------
011600010330     D WLBDA8          DS                  INZ
011700010330     D  G08DAT                 1      8  0
011800010330     D  G08INV                 9     16  0
011900010330     D  G08ERR                17     17
012000010330     D  G08TGI                18     22  0
012100031201     D*------------------
012200031201     D* Costanti
012300031201     D*------------------
012400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
012500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
012600010330
012700010330
012800010330
012900990915     C                   time                    wrkdata
013000990915     C                   time                    wrkora
013100000913     C                   reset                   rrnum
013200990921     C                   reset                   esito
013300990921     C                   reset                   wrkesito
013400010601     C*
013500010601     C                   exsr      opeini
013600010605     C                   exsr      rwvab
013700010601     C*
013800010601     C                   seton                                        lr
013900010601
014000010601
014100010601
014200010601     C*--------------------------------------------------------
014300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014400010601     C*--------------------------------------------------------
014500010601     C     PREELA        BEGSR
014600010601     C*
014700010601     C* SE OCCORRE SPEDIRE IN FILIALE
014800010601     C                   if        invfil <> *zeros and
014900010601     C                             flgGiro = '0'
015000010601     C*
015100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015200010601     C                   eval      flgGiro = '1'
015300000724     C*
015400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
015500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015600010601     C     invfil        chain     azorg01l
015700000724     C                   if        %found
015800000724     C                   movel(p)  CMD(1)        depcmd
015900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
016000000724     C*
016100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
016200011113     C                   Z-ADD     150           LENGH            15 5
016300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016400000724     C                   PARM                    depcmd
016500000724     C                   PARM                    LENGH
016600000724     C*
016700000724     C                   endif
016800000616     C*
016900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017000000616     C                   movel(p)  CMD(2)        depcmd
017100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
017200000616     C*
017300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017400011113     C                   Z-ADD     150           LENGH            15 5
017500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017600000616     C                   PARM                    depcmd
017700000616     C                   PARM                    LENGH
017800000613     C*
017900040122     C                   if        not %open(titv92ps)
018000040122     C                   open      titv92ps
018100070329     C                   except    testdett
018200000613     C                   endif
018300010601     C*
018400010601     C                   endif
018500010601     C*
018600010601     C                   ENDSR
018700010601     C***
018800010601
018900010601
019000010601
019100010601
019200010601     C*--------------------------------------------------------
019300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
019400010601     C*--------------------------------------------------------
019500010601     C     ENDELA        BEGSR
019600990915     C*
019700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
019800040119     C*
019900040119     C* Effettuo la chiamata al CLLE preposto
020000040119     C                   call(e)   'TITVVTC'
020100040510     C                   parm                    parccm
020200040119     C                   parm                    parmbr
020300040119     C                   parm      '2'           paropz
020400050201     C*
020500050201     C* Effettuo lancio TISI95 solo x chiusura
020600050201     C                   CLEAR                   TISI95DS
020700050201     C                   EVAL      I95TLA = 'C'
020800050201     C                   CALL      'TISI95R'
020900050201     C                   PARM                    TISI95DS
021000000613     C*
021100040122     C                   if        %open(titv92ps)
021200000616     C                   except    findett
021300040122     C                   close     titv92ps
021400000613     C                   endif
021500000616     C*
021600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
021700011113     C                   Z-ADD     150           LENGH            15 5
021800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
021900000616     C                   PARM                    CMD(3)
022000000616     C                   PARM                    LENGH
022100080506     C*
022200080506     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
022300080506     C   54              call(e)   'TITV84R'
022400080506     C                   parm      'C'           InTipl
022500080506     C                   parm                    wtivlrds
022600080506     C                   parm                    wfivabds
022700000616     C*
022800010601     C                   ENDSR
022900010601     C***
023000010601
023100010601
023200010601
023300000613
023400000613     C*--------------------------------------------------------
023500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023600000613     C*--------------------------------------------------------
023700000621     C     STPR          BEGSR
023800000613     C*
023900040122     C                   if        not %open(titv92p)
024000040122     C   50              open      titv92p
024100990915     C                   endif
024200990915     C*
024300030715     C   50              except    riepilogo
024400990915     C*
024500040122     C                   if        %open(titv92p)
024600040122     C   50              close     titv92p
024700990914     C                   endif
024800000613     C*
024900000613     C                   ENDSR
025000000613     C***
025100000613
025200010601
025300010601
025400010330     C*--------------------------------------------------------
025500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
025600010330     C*--------------------------------------------------------
025700010605     C     RWVAB         BEGSR
025800010330     C*
025900010330     C                   if        not %open(tivin00r)
026000010330     C                   open      tivin00r
026100010330     C                   endif
026200021025     C                   if        not %open(fivabwwr)
026300021025     C                   open      fivabwwr
026400010330     C                   endif
026500040119     C*
026600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
026700040119     C                   exsr      prevat
026800040119     C*
026900040119     C                   if        chkcall = '0'
027000040119     C*
027100040119     C                   if        not %open(fivatwwr)
027200040119     C                   open      fivatwwr
027300040119     C                   endif
027400010330     C*
027500040119     C                   clear                   §CTROKVB          7 0
027600040119     C                   clear                   §CTROKVT          7 0
027700040119     C                   clear                   §CTRMO            7 0
027800040119     C                   clear                   §CTRNO            7 0
027900010330     C*
028000030822     C                   do        *HIVAL
028100010330     C*
028200030822     C                   read      tivin00r                               70
028300010618     C*
028400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
028500010618     C                   if        vinflg = '1'
028600010618     C                   eval      flgOk = '1'
028700010618     C                   endif
028800040510     C*
028900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
029000040510     C                   z-add     *zeros        wLenVINDTA        4 0
029100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
029200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
029300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
029400090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
029500040510     C                   z-add     1             wFlgVINDTA
029600040510     C                   leave
029700040510     C                   endif
029800040510     C                   enddo
029900010618     C*
030000040510     C                   if        vindta > *blanks AND
030100040510     C                             wFlgVINDTA = 1
030200010330     C                   add       1             rrnum
030300010330     C*
030400010601     C                   if        *in70 = *off and
030500010330     C                             (vinflg = *blanks
030600010330     C                              or vinflg = '0'
030700010330     C                              or vinflg = '2')
030800010330     C*
030900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
031000010711     C                   if        vinflg = *blanks or vinflg = '0'
031100010711     C                   clear                   vinmsg
031200010711     C                   endif
031300010601     C*
031400010330     C                   exsr      inzvar
031500090403     C*
031600090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
031700090403     C* separatori campi
031800090403     C                   if        CharTXT <> *blanks
031900090403     C                   z-add     *zeros        wLenVINDTA        4 0
032000090403     C                   z-add     *zeros        wFlgVINDTA        1 0
032100090403     C                   setoff                                       80
032200090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
032300090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
032400090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
032500090403     C                   if        not *in80
032600090403     C                   seton                                        80
032700090403     C                   else
032800090403     C                   setoff                                       80
032900090403     C                   endif
033000090403     C                   endif
033100090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
033200090403     C                             and *in80 = *on
033300090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
033400090403     C                   endif
033500090403     C                   enddo
033600090403     C                   endif
033700090403     C*
033800010605     C                   exsr      impvab
033900010601     C*
034000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
034100010605     C                   eval      depfil = VABLNP
034200010601     C                   exsr      repfil
034300010601     C                   if        depfil = invfil
034400021025     C                   if        vlrpoi = 999
034500030822     C                   move(P)   invfil        VABFGS
034600070704     C                   move(P)   invfil        winvcmz           3
034700021025     C                   else
034800030822     C                   move(P)   vlrpoi        VABFGS
034900070704     C                   move(P)   vlrpoi        winvcmz
035000021025     C                   endif
035100010601     C*
035200010601     C                   exsr      PREELA
035300010601     C*
035400010604     C* Ebbene...
035500010604     C*
035600040119     C  N31              add       1             §CTROKVB
035700040119     C   32              add       1             §CTRMO
035800040119     C   31              add       1             §CTRNO
035900080506     C*
036000030822     C                   if        wGiro = 2
036100040510     C* Scrivo sempre il VAT
036200040510     C  N31              exsr      wrivat
036300040510     C* Scrivo il VAB solo a rottura d spedizione
036400040119     C                   if        depspe =  *zeros OR
036500040119     C                             depspe <> vabNSP
036600040119     C  N31              exsr      wrivab
036700040119     C  N31              eval      depspe = vabNSP
036800040119     C                   endif
036900040119     C                   endif
037000010604     C*
037100010604     C                   if        *in31 = *off and
037200010604     C                             *in32 = *off
037300010604     C                   eval      vinflg = '1'
037400010604     C                   else
037500010604     C                   eval      recko = vindta
037600010604     C                   except    rigadett
037700010604     C                   eval      vinflg = '2'
037800010604     C                   endif
037900010604     C                   endif
038000010601     C*
038100010604     C                   endif
038200010604     C*
038300010330     C                   else
038400010330     C                   eval      vinflg = '1'
038500010330     C                   endif
038600010601     C*
038700010601     C  N70              update    tivin000
038800010330     C*
038900030822     C  N70              enddo
039000040119     C*
039100040119     C                   endif                                                  (endif - chkcall)
039200010601     C*
039300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
039400010601     C                   if        cntNonEl = *zeros or
039500010601     C                             flgMulti = '0'
039600010330     C* Se non ci sono record con errori ...
039700010601     C                   if        §ctrno = 0 and
039800010604     C                             §ctrmo = 0 and
039900010601     C                             flgStato <> '2'
040000010330     C* ... restituisco esito OK.
040100010330     C                   eval      wrkesito = '0'
040200010330     C                   else
040300040119     C                   if        §ctrokvb > 0
040400010330     C                   eval      wrkesito = '1'
040500010330     C                   else
040600010615     C                   if        flgOk = '0'
040700010615     C                   eval      wrkesito = '2'
040800010615     C                   else
040900010615     C                   eval      wrkesito = '6'
041000010615     C                   endif
041100010330     C                   endif
041200010330     C                   endif
041300010601     C                   else
041400010601     C                   eval      wrkesito = '9'
041500010601     C                   endif
041600010330     C*
041700010330     C                   if        %open(tivin00r)
041800010330     C                   close     tivin00r
041900010330     C                   endif
042000021025     C                   if        %open(fivabwwr)
042100021025     C                   close     fivabwwr
042200010330     C                   endif
042300040512     C                   if        %open(fivatwwr)
042400040512     C                   close     fivatwwr
042500040512     C                   endif
042600010601     C*
042700010601     C                   if        vlrpoi <> 999
042800010601     C                   eval      invfil = vlrpoi
042900010601     C                   endif
043000010330     C*
043100040119     C                   if        §ctrokvb > 0
043200010601     C                             and invfil > *zeros
043300010330     C                   exsr      invio
043400010330     C                   endif
043500010601     C*
043600010618     C                   if        flgGiro = '1'
043700010601     C                   exsr      endela
043800010618     C                   endif
043900010330     C*
044000010330     C                   ENDSR
044100010330     C***
044200040119
044300040119
044400040119
044500040119     C*----------------------------------------------------*
044600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
044700040119     C*----------------------------------------------------*
044800040510     C     WRIVAB        BEGSR
044900040119     C*
045000060331     C* Gestisco l'eventuale rottura x numero spedizione
045100060331     C                   if        VABNSP <> wVABNSP
045200070301     C                   if        wVATNOT_E <> *blanks
045300070301     C                   eval      VABCTM = '7Q'
045400070301     C                   endif
045500080506     C*
045600080506     C* Se presente particolarità consegna 'RC' e se il codice
045700080506     C* cliente mittente è valorizzato e numero serie ORM nn passata
045800080506     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
045900080506     C                   IF        VABGMA = 'RC'
046000080506     C                   EVAL      wtivlrds = tivlrds
046100080506     C                   EVAL      w_vlrppt = *blanks
046200080506     C                   IF        VABCCM > *ZEROS
046300080506     C                   IF        wNSR = *BLANKS
046400080506     C                   Z-ADD     1             jOSR
046500080506     C                   move(p)   VABLNP        wLnpOSR
046600080506     C                   move(p)   VABCCM        wCliOSR
046700080506     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
046800080506     C                   IF        %found
046900080506     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
047000080506     C                   EVAL      %subst(w_vlrppt:4:2) = wNSR
047100080506     C                   ELSE
047200080506     C                   SETON                                        31
047300080506     C                   EVAL      vinmsg = %trimr(vinmsg)
047400080506     C                             + ' ' + 'ORMNRS(tabellaOSR)'
047500080506     C                   ENDIF
047600080506     C                   ENDIF
047700080506     C                   ENDIF
047800080506     C                   ENDIF
047900080506     C*
048000080506     C  N31              write     fivab000                                     => scarico il VAB
048100080506     C*
048200080506     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
048300080506     C                   if        *IN31 = *OFF AND
048400080506     C                             vabGMA = 'RC'
048500080506     C*
048600080506     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
048700080506     C                   seton                                        54
048800080506     C*
048900080506     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
049000080506     C                   eval      wfivabds = fivabds
049100080506     C                   call      'TITV84R'
049200080506     C                   parm      *blanks       InTipl            1
049300080506     C                   parm                    wtivlrds
049400080506     C                   parm                    wfivabds
049500080506     C                   endif
049600080506     C*
049700060331     C                   eval      wVABNSP = VABNSP
049800060331     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100040119
050200040119
050300040119
050400040119     C*----------------------------------------------------*
050500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
050600040119     C*----------------------------------------------------*
050700040510     C     WRIVAT        BEGSR
050800060512     C*
050900060512     C* Inizializzo i campi chiave
051000060512     C                   eval      VATFGS = VABFGS
051100060512     C                   eval      VATAAS = VABAAS
051200060512     C                   eval      VATCCM = VABCCM
051300060512     C                   eval      VATNSP = VABNSP
051400060512     C                   eval      VATLNP = VABLNP
051500040714     C*
051600040714     C* Solo se VATNOT valorizzato scarico il buffer
051700040714     C                   if        VATNOT <> *blanks
051800040119     C*
051900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
052000040510     C                   if        VATTRC = *blanks
052100040510     C                   eval      VATTRC = 'E'
052200040510     C                   endif
052300040510     C                   write     FIVAT000
052400040119     C                   add       1             §CTROKVT
052500040714     C*
052600040714     C                   endif
052700060331     C*
052800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
052900060331     C                   if        wVATNOT_A <> *blanks
053000060331     C                   eval      VATNOT = wVATNOT_A
053100060331     C                   eval      VATTRC = 'A'
053200060331     C                   write     FIVAT000
053300060331     C                   add       1             §CTROKVT
053400060331     C                   endif
053500060331     C*
053600060331     C                   if        wVATNOT_B <> *blanks
053700060331     C                   eval      VATNOT = wVATNOT_B
053800060331     C                   eval      VATTRC = 'B'
053900060331     C                   write     FIVAT000
054000060331     C                   add       1             §CTROKVT
054100060331     C                   endif
054200060331     C*
054300060331     C                   if        wVATNOT_E <> *blanks
054400060331     C                   eval      VATNOT = wVATNOT_E
054500060331     C                   eval      VATTRC = 'E'
054600060331     C                   write     FIVAT000
054700060331     C                   add       1             §CTROKVT
054800060331     C                   endif
054900110912     C*
055000110912     C                   if        wVATNOT_I <> *blanks
055100110912     C                   eval      VATNOT = wVATNOT_I
055200110912     C                   eval      VATTRC = 'I'
055300110912     C                   write     FIVAT000
055400110912     C                   add       1             §CTROKVT
055500110912     C                   endif
055600110912     C*
055700110912     C                   if        wVATNOT_J <> *blanks
055800110912     C                   eval      VATNOT = wVATNOT_J
055900110912     C                   eval      VATTRC = 'J'
056000110912     C                   write     FIVAT000
056100110912     C                   add       1             §CTROKVT
056200110912     C                   endif
056300110912     C*
056400110912     C                   if        wVATNOT_4 <> *blanks
056500110912     C                   eval      VATNOT = wVATNOT_4
056600110912     C                   eval      VATTRC = '4'
056700110912     C                   write     FIVAT000
056800110912     C                   add       1             §CTROKVT
056900110912     C                   endif
057000131018     C*
057100131018     C                   if        wVATNOT_S <> *blanks
057200131018     C                   eval      VATNOT = wVATNOT_S
057300131018     C                   eval      VATTRC = 'S'
057400131018     C                   write     FIVAT000
057500131018     C                   add       1             §CTROKVT
057600131018     C                   endif
057700141128     C*
057800141128     C                   if        wVATNOT_Y <> *blanks
057900141128     C                   eval      VATNOT = wVATNOT_Y
058000141128     C                   eval      VATTRC = 'Y'
058100141128     C                   write     FIVAT000
058200141128     C                   add       1             §CTROKVT
058300141128     C                   endif
058400040119     C*
058500040119     C                   ENDSR
058600040119
058700040119
058800040119
058900040119     C*----------------------------------------------------*
059000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
059100040119     C*----------------------------------------------------*
059200040119     C     PREVAT        BEGSR
059300040119     C*
059400040119     C* Compongo il nome del membro da dare al FIVATWWR
059500040119     C                   eval      parmbr = vlrhdl
059600040119     C                   movel     'M'           parmbr
059700040510     C                   eval      parccm = vlrksc
059800040119     C                   eval      paropz = '1'
059900040119     C* Effettuo la chiamata al CLLE preposto
060000040119     C                   call(e)   'TITVVTC'
060100040119     C                   parm                    parccm
060200040119     C                   parm                    parmbr
060300040119     C                   parm                    paropz
060400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060500040119     C                   if        %error
060600040119     C                   movel     '1'           chkcall
060700040119     C                   else
060800040119     C                   movel     '0'           chkcall
060900040119     C                   endif
061000040119     C*
061100040119     C                   ENDSR
061200010601
061300010601
061400010601
061500010330     C*----------------------------------------------------*
061600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
061700010330     C*----------------------------------------------------*
061800040119     C     INZVAR        BEGSR
061900010330     C*
062000030822     C* Inizializzo variabili di wrk
062100020204     C                   Z-ADD     *zeros        Num5_0            5 0
062200090403     C                   z-add     *zeros        invfil
062300060331     C                   MOVEL     *blanks       wVATNOT_A
062400060331     C                   MOVEL     *blanks       wVATNOT_B
062500060331     C                   MOVEL     *blanks       wVATNOT_E
062600110912     C                   MOVEL     *blanks       wVATNOT_I
062700110912     C                   MOVEL     *blanks       wVATNOT_J
062800110912     C                   MOVEL     *blanks       wVATNOT_4
062900131018     C                   MOVEL     *blanks       wVATNOT_S
063000141128     C                   MOVEL     *blanks       wVATNOT_Y
063100030822     C*
063200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
063300030822     C                   CLEAR                   FIVAB000
063400040119     C                   CLEAR                   FIVAT000
063500030822     C                   CLEAR                   SkSplitCSV
063600030822     C                   EVAL      i = 1
063700030822     C                   EVAL      posDa = *zeros
063800030822     C                   EVAL      posA  = *zeros
063900030822     C*
064000030822     C* Reimposto i valori di default
064100030822     C                   EXSR      DEFCAM
064200030822     C*
064300010330     C                   ENDSR
064400010330     C*----------------------------------------------------*
064500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
064600010330     C*----------------------------------------------------*
064700010330     C     DEFCAM        BEGSR
064800030822     C*
064900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
065000030715     C* e delimitatore testo.
065100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
065200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
065300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
065400090403     C*
065500090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
065600090403     C                   EVAL      CharSOS = CharNUM
065700030822     C*
065800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
065900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
066000030715     C                   SETON                                        50
066100030715     C                   ELSE
066200030715     C                   SETOFF                                       50
066300030715     C                   ENDIF
066400030822     C*
066500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
066600030822     C                   EVAL      posDaDft = 1
066700030822     C                   EVAL      posADft  = 0
066800030822     C                   EVAL      wGiroDft = 0
066900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
067000030822     C                             posDaDft > 0
067100030822     C*
067200030822     C* Gestisco il 1° giro
067300030822     C                   IF        wGiroDft = 0
067400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
067500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
067600030822     C* Incremento il contatore dei "giri"
067700030822     C                   EVAL      wGiroDft = 1
067800030822     C                   ELSE
067900030822     C                   EVAL      posDaDft = posADft
068000030822     C                   ENDIF
068100030822     C* Eseguo lo scan x trovare la fine del campo corrente
068200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
068300030822     C*
068400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
068500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
068600030822     C                   IF        posDaDft > 0 AND
068700030822     C                             posADft  > 0
068800030822     C* NCL
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'NCL'
069300030822     C                   EVAL      PiStr=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   EXSR      CHKNUM
069700030822     C                   IF        PiInt=*on
069800030822     C                   Z-ADD     PiVal         VABNCL
069900030822     C                   ENDIF
070000030822     C                   ENDIF
070100030822     C* CCM
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'CCM'
070600030822     C                   EVAL      PiStr=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   EXSR      CHKNUM
071000030822     C                   IF        PiInt=*on
071100030822     C                   Z-ADD     PiVal         VABCCM
071200030822     C                   ENDIF
071300030822     C                   ENDIF
071400030822     C* LNP
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'LNP'
071900030822     C                   EVAL      PiStr=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   EXSR      CHKNUM
072300030822     C                   IF        PiInt=*on
072400030822     C                   Z-ADD     PiVal         VABLNP
072500030822     C                   ENDIF
072600030822     C                   ENDIF
072700030822     C* NRS
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'NRS'
073200030822     C                   EVAL      PiStr=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   EXSR      CHKNUM
073600030822     C                   IF        PiInt=*on
073700030822     C                   Z-ADD     PiVal         VABNRS
073800030822     C                   ENDIF
073900030822     C                   ENDIF
074000030822     C* CTR
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'CTR'
074500030822     C                   EVAL      PiStr=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   EXSR      CHKNUM
074900030822     C                   IF        PiInt=*on
075000030822     C                   Z-ADD     PiVal         VABCTR
075100030822     C                   ENDIF
075200030822     C                   ENDIF
075300030822     C* PKB
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'PKB'
075800030822     C                   EVAL      PiStr=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   EXSR      CHKNUM
076200030822     C                   IF        PiNum=*on
076300030822     C                   Z-ADD     PiVal         VABPKB
076400030822     C                   ENDIF
076500030822     C                   ENDIF
076600030822     C* VLB
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'VLB'
077100030822     C                   EVAL      PiStr=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   EXSR      CHKNUM
077500030822     C                   IF        PiNum=*on
077600030822     C                   Z-ADD     PiVal         VABVLB
077700030822     C                   ENDIF
077800030822     C                   ENDIF
077900030822     C* QFT
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'QFT'
078400030822     C                   EVAL      PiStr=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   EXSR      CHKNUM
078800030822     C                   IF        PiNum=*on
078900030822     C                   Z-ADD     PiVal         VABQFT
079000030822     C                   ENDIF
079100030822     C                   ENDIF
079200030822     C* CBO
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'CBO'
079700030822     C                   EVAL      VABCBO=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* TSP
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'TSP'
080600030822     C                   EVAL      VABTSP=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* VAS
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'VAS'
081500030822     C                   EVAL      VABVAS=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* VCA
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'VCA'
082400030822     C                   EVAL      VABVCA=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* GCA
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'GCA'
083300030822     C                   EVAL      VABGCA=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* CTM
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'CTM'
084200030822     C                   EVAL      VABCTM=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600030822     C* FFD
084700030822     C                   IF        %subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):1:3)
085000030822     C                             = 'FFD'
085100030822     C                   EVAL      VABFFD=%trim(%subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400030822     C                   ENDIF
085500030822     C* VAD
085600030822     C                   IF        %subst(
085700030822     C                             %subst(vlrppt:posDaDft+1:
085800030822     C                             posADft-posDaDft-1):1:3)
085900030822     C                             = 'VAD'
086000030822     C                   EVAL      VABVAD=%trim(%subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086300030822     C                   ENDIF
086400030822     C* GMA
086500030822     C                   IF        %subst(
086600030822     C                             %subst(vlrppt:posDaDft+1:
086700030822     C                             posADft-posDaDft-1):1:3)
086800030822     C                             = 'GMA'
086900030822     C                   EVAL      VABGMA=%trim(%subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087200030822     C                   ENDIF
087300030822     C* GGA
087400030822     C                   IF        %subst(
087500030822     C                             %subst(vlrppt:posDaDft+1:
087600030822     C                             posADft-posDaDft-1):1:3)
087700030822     C                             = 'GGA'
087800030822     C                   EVAL      VABGGA=%trim(%subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088100030822     C                   ENDIF
088200030822     C* GVA
088300030822     C                   IF        %subst(
088400030822     C                             %subst(vlrppt:posDaDft+1:
088500030822     C                             posADft-posDaDft-1):1:3)
088600030822     C                             = 'GVA'
088700030822     C                   EVAL      VABGVA=%trim(%subst(
088800030822     C                             %subst(vlrppt:posDaDft+1:
088900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089000030822     C                   ENDIF
089100030822     C* TC1
089200030822     C                   IF        %subst(
089300030822     C                             %subst(vlrppt:posDaDft+1:
089400030822     C                             posADft-posDaDft-1):1:3)
089500030822     C                             = 'TC1'
089600030822     C                   EVAL      VABTC1=%trim(%subst(
089700030822     C                             %subst(vlrppt:posDaDft+1:
089800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089900030822     C                   ENDIF
090000030822     C* TC2
090100030822     C                   IF        %subst(
090200030822     C                             %subst(vlrppt:posDaDft+1:
090300030822     C                             posADft-posDaDft-1):1:3)
090400030822     C                             = 'TC2'
090500030822     C                   EVAL      VABTC2=%trim(%subst(
090600030822     C                             %subst(vlrppt:posDaDft+1:
090700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090800030822     C                   ENDIF
090900080516     C* TIC
091000080516     C                   IF        %subst(
091100080516     C                             %subst(vlrppt:posDaDft+1:
091200080516     C                             posADft-posDaDft-1):1:3)
091300080516     C                             = 'TIC'
091400080516     C                   EVAL      VABTIC=%trim(%subst(
091500080516     C                             %subst(vlrppt:posDaDft+1:
091600080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
091700080516     C                   ENDIF
091800040714     C* VATTRC
091900040714     C                   IF        %subst(
092000040714     C                             %subst(vlrppt:posDaDft+1:
092100040714     C                             posADft-posDaDft-1):1:3)
092200040714     C                             = 'TRC'
092300040714     C                   EVAL      VATTRC=%trim(%subst(
092400040714     C                             %subst(vlrppt:posDaDft+1:
092500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
092600040714     C                   ENDIF
092700030822     C* ...
092800030822     C                   ENDIF
092900030822     C                   ENDDO
093000020204     C*
093100010330     C                   ENDSR
093200010607     C*----------------------------------------------------*
093300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
093400010607     C*----------------------------------------------------*
093500010607     C     IMPVAB        BEGSR
093600010607     C*
093700010607     C                   SETOFF                                       3132
093800030515     C*
093900010607     C                   MOVEL     datcor        VABAAS
094000010607     C                   MOVE      datcor        VABMGS
094100010607     C*
094200030715     C**********
094300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
094400030715     C**********
094500030715     C                   IF        wGiro = *zeros
094600040113     C*
094700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
094800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
094900040510     C                             %trim(intcol(3))<>*blanks
095000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
095100040113     C                                      %trim(intcol(3))
095200040510     C                   ENDIF
095300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
095400031201     C     minu:maiu     XLATE     vindta        vindta
095500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095600030715     C                   DOW       posDa <= %len(%trim(vindta))
095700030715     C*
095800030715     C* Gestisco il 1° campo
095900030715     C                   IF        i = 1
096000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096100030715     C                   EVAL      posDa = 1
096200030715     C* Eseguo lo scan x trovare la fine del primo campo
096300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096400030715     C* A questo punto "estraggo" il campo corrente
096500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096600030715     C                                                    (posA-posDa))
096700030715     C* X i campi successivi al 1°
096800030715     C                   ELSE
096900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097000030715     C                   EVAL      posDa = posA + 1
097100030715     C* Eseguo lo scan x trovare la fine del campo corrente
097200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097300030715     C* Gestisco l'ultimo campo
097400030715     C                   IF        posA = *zeros
097500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097600030715     C* A questo punto "estraggo" il campo corrente
097700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
097800030715     C                                                    (posA-posDa))
097900030715     C                   LEAVE
098000030715     C                   ELSE
098100030715     C* A questo punto "estraggo" il campo corrente
098200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
098300030715     C                                                    (posA-posDa))
098400030715     C                   ENDIF
098500030715     C                   ENDIF
098600030715     C* Incremento il contatore d campo
098700030715     C                   EVAL      i = i +1
098800030715     C                   ENDDO
098900030715     C*
099000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099100030715     C* delimitazione testo
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitFLD)
099400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
099500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
099600030715     C                   EVAL      i = i + 1
099700030715     C                   ENDDO
099800030715     C                   EVAL      wGiro = 1
099900030715     C                   ELSE
100000030715     C                   EVAL      wGiro = 2
100100031204     C**********
100200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
100300031204     C* d separatore campo
100400031204     C**********
100500031204     C                   EVAL      vindta = %trim(vindta)
100600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
100700031204     C                   EVAL      vindta = %subst(vindta:2)
100800031204     C                   ENDDO
100900031204     C*
101000031204     C                   Z-ADD     *zeros        lunghInput        4 0
101100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
101200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
101300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
101400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
101500031204     C                   ENDDO
101600030715     C**********
101700030715     C* Effettuo lo split del campo dati d input
101800030715     C**********
101900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
102000030715     C                   DOW       posDa <= %len(%trim(vindta))
102100030715     C*
102200030715     C* Gestisco il 1° campo
102300030715     C                   IF        i = 1
102400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
102500030715     C                   EVAL      posDa = 1
102600030715     C* Eseguo lo scan x trovare la fine del primo campo
102700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102800030715     C* A questo punto "estraggo" il campo corrente
102900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103000030715     C                                                    (posA-posDa))
103100030715     C* X i campi successivi al 1°
103200030715     C                   ELSE
103300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
103400030715     C                   EVAL      posDa = posA + 1
103500031204     C* Verifico che nn vi sia il campo nn valorizzato
103600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
103700031204     C* Se campo nn valorizzato skippo al prossimo
103800031204     C                   EVAL      SkSplitCSV(i) = *blanks
103900031204     C                   EVAL      posA  = posA + 1
104000031204     C                   ELSE
104100030715     C* Eseguo lo scan x trovare la fine del campo corrente
104200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
104300030715     C* Gestisco l'ultimo campo
104400030715     C                   IF        posA = *zeros
104500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
104600030715     C* A questo punto "estraggo" il campo corrente
104700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
104800030715     C                                                    (posA-posDa))
104900030715     C                   LEAVE
105000030715     C                   ELSE
105100030715     C* A questo punto "estraggo" il campo corrente
105200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
105300030715     C                                                    (posA-posDa))
105400030715     C                   ENDIF
105500031204     C                   ENDIF
105600030715     C                   ENDIF
105700030715     C* Incremento il contatore d campo
105800030715     C                   EVAL      i = i +1
105900030715     C                   ENDDO
106000030715     C*
106100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
106200030715     C* delimitazione testo
106300030715     C                   EVAL      i = 1
106400030715     C                   DOW       i < %elem(SkSplitCSV)
106500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
106600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
106700030715     C                   EVAL      i = i + 1
106800030715     C                   ENDDO
106900030715     C*
107000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
107100030715     C                   EVAL      i = 1
107200030715     C                   DOW       i < %elem(SkSplitFLD)
107300030715     C*
107400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
107500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
107600030715     C                             SkSplitCSV(i) <> *blanks
107700030721     C*** ATB
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
107900030721     C                   EVAL      VABATB = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030715     C*** RSD
108200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
108300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
108400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
108500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
108600030715     C* ==
108700030715     C                   ENDIF
108800030721     C*** RD2
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
109000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030715     C*** IND
109300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
109400030715     C                   EVAL      VABIND = SkSplitCSV(i)
109500030715     C                   ENDIF
109600030715     C*** LOD
109700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
109800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
109900030715     C                   ENDIF
110000030715     C*** PRD
110100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
110200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
110300030715     C                   ENDIF
110400030721     C*** NZD
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
110600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030715     C*** RMA
110900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
111000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
111100030715     C                   ENDIF
111200030715     C*** NOT
111300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
111400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
111500030715     C                   ENDIF
111600030715     C*** NT2
111700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
111800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
111900030715     C                   ENDIF
112000030715     C*** VCA
112100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
112200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
112300030715     C                   ENDIF
112400030715     C*** VAS
112500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
112600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
112700030715     C                   ENDIF
112800030715     C*** FFD
112900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
113000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
113100030715     C                   ENDIF
113200030721     C*** GC1
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
113400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
113500100304     C                   IF        %subst(VABGC1:1) = 'P' or
113600100304     C                             %subst(VABGC1:1) = 'M'
113700100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
113800100304     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** GC2
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
114200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
114300100304     C                   IF        %subst(VABGC2:1) = 'P' or
114400100304     C                             %subst(VABGC2:1) = 'M'
114500100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
114600100304     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** TSP
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
115000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
115100030721     C                   ENDIF
115200040510     C*** CBO
115300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
115400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
115500040510     C                   ENDIF
115600030721     C*** NAS
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
115800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
115900030721     C                   ENDIF
116000030721     C*** TIC
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
116200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
116300030721     C                   ENDIF
116400030721     C*** GCA
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
116600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
116700030721     C                   ENDIF
116800030721     C*** XCO
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
117000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
117100030721     C                   ENDIF
117200030721     C*** CTM
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
117400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
117500030721     C                   ENDIF
117600030721     C*** TCR
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
117800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
117900030721     C                   ENDIF
118000030721     C*** CTS
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
118200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
118300030721     C                   ENDIF
118400030721     C*** FTM
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
118600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
118700030721     C                   ENDIF
118800030721     C*** VAD
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
119000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
119100030721     C                   ENDIF
119200030721     C*** GMA
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
119400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
119500030721     C                   ENDIF
119600030721     C*** GGA
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
119800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
119900030721     C                   ENDIF
120000030721     C*** GVA
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
120200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
120300030721     C                   ENDIF
120400030721     C*** TC1
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
120600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
120700030721     C                   ENDIF
120800030721     C*** TC2
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
121000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
121100030721     C                   ENDIF
121200030721     C*** SCL
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
121400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
121500030721     C                   ENDIF
121600030721     C*** RMO
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
121800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
121900030721     C                   ENDIF
122000030721     C*** NMO
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
122200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
122300030721     C                   ENDIF
122400030721     C*
122500030721     C* Reperisco quindi i campi numerici...
122600040510     C*** NSP
122700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
122800040510     C                   EVAL      PiStr=SkSplitCSV(i)
122900040510     C                   EXSR      CHKNUM
123000040510     C                   IF        PiInt=*on
123100040510     C                   Z-ADD     PiVal         VABNSP
123200040510     C                   ELSE
123300040510     C                   SETON                                        31
123400040510     C                   EVAL      VABNSP = *zeros
123500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040510     C                             + ' ' + 'VABNSP'
123700040510     C                   ENDIF
123800040510     C                   ENDIF
123900070703     C*** RMN
124000070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
124100070703     C                   EVAL      PiStr=SkSplitCSV(i)
124200070703     C                   EXSR      CHKNUM
124300070703     C                   IF        PiInt=*on
124400070703     C                   Z-ADD     PiVal         VABRMN
124500070703     C                   ELSE
124600070703     C                   SETON                                        32
124700070703     C                   EVAL      vinmsg = %trimr(vinmsg)
124800070703     C                             + ' ' + 'VABRMN'
124900070703     C                   ENDIF
125000070703     C                   ENDIF
125100070705     C*** CAD
125200070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
125300070705     C                   EVAL      VABCAD = SkSplitCSV(i)
125400070705     C                   ENDIF
125500030721     C*** NCL
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on
126000030721     C                   Z-ADD     PiVal         VABNCL
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      VABNCL = *zeros
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500040122     C                             + ' ' + 'VABNCL'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** PKB
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300030721     C                   Z-ADD     PiVal         VABPKB
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      VABPKB = *zeros
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABPKB'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** CAS
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiNum=*on
128600030721     C                   Z-ADD     PiVal         VABCAS
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      VABCAS = *zeros
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABCAS'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** CCM
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on      AND
129900030721     C                             PiVal<=9999999 AND
130000030721     C                             PiVal>*zeros
130100030721     C                   Z-ADD     PiVal         VABCCM
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      VABCCM = *zeros
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600040510     C                             + ' ' + 'VABCCM'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** LNP
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on    AND
131400030721     C                             PiVal<=999   AND
131500030721     C                             PiVal>*zeros
131600030721     C                   Z-ADD     PiVal         VABLNP
131700030721     C                   ELSE
131800040715     C                   SETON                                        32
131900030721     C                   EVAL      VABLNP = *zeros
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100040510     C                             + ' ' + 'VABLNP'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** NRS
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on AND
132900030721     C                             PiVal<=99
133000030721     C                   Z-ADD     PiVal         VABNRS
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABNRS'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** LNA
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on    AND
134200030721     C                             PiVal<=999   AND
134300030721     C                             PiVal>*zeros
134400030721     C                   Z-ADD     PiVal         VABLNA
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      VABLNA = *zeros
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABLNA'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** CTR
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on  AND
135700030721     C                             PiVal<=999
135800030721     C                   Z-ADD     PiVal         VABCTR
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABCTR'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** IAS
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiNum=*on
137000030721     C                   Z-ADD     PiVal         VABIAS
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABIAS'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** VLB
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiNum=*on
138200030721     C                   Z-ADD     PiVal         VABVLB
138300030721     C                   ELSE
138400030721     C                   SETON                                        32
138500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138600030721     C                             + ' ' + 'VABVLB'
138700030721     C                   ENDIF
138800030721     C                   ENDIF
138900030721     C*** QFT
139000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
139100030721     C                   EVAL      PiStr=SkSplitCSV(i)
139200030721     C                   EXSR      CHKNUM
139300030721     C                   IF        PiNum=*on
139400030721     C                   Z-ADD     PiVal         VABQFT
139500030721     C                   ELSE
139600030721     C                   SETON                                        32
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABQFT'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100030721     C*** NCD
140200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiInt=*on      AND
140600030721     C                             PiVal<=9999999
140700030721     C                   Z-ADD     PiVal         VABNCD
140800030721     C                   ELSE
140900030721     C                   SETON                                        32
141000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141100030721     C                             + ' ' + 'VABNCD'
141200030721     C                   ENDIF
141300030721     C                   ENDIF
141400030721     C*** NCA
141500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
141600030721     C                   EVAL      PiStr=SkSplitCSV(i)
141700030721     C                   EXSR      CHKNUM
141800030721     C                   IF        PiInt=*on      AND
141900030721     C                             PiVal<=9999999
142000030721     C                   Z-ADD     PiVal         VABNCA
142100030721     C                   ELSE
142200030721     C                   SETON                                        32
142300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142400030721     C                             + ' ' + 'VABNCA'
142500030721     C                   ENDIF
142600030721     C                   ENDIF
142700030721     C*** ZNC
142800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
142900030721     C                   EVAL      PiStr=SkSplitCSV(i)
143000030721     C                   EXSR      CHKNUM
143100030721     C                   IF        PiInt=*on AND
143200030721     C                             PiVal<=99
143300030721     C                   Z-ADD     PiVal         VABZNC
143400030721     C                   ELSE
143500030721     C                   SETON                                        32
143600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143700030721     C                             + ' ' + 'VABZNC'
143800030721     C                   ENDIF
143900030721     C                   ENDIF
144000030721     C*** DCR
144100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
144200030721     C                   EVAL      PiStr=SkSplitCSV(i)
144300030721     C                   EXSR      CHKNUM
144400030721     C                   IF        PiInt=*on       AND
144500030721     C                             PiVal<=99999999
144600030721     C                   Z-ADD     PiVal         VABDCR
144700030721     C                   ELSE
144800030721     C                   SETON                                        32
144900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145000030721     C                             + ' ' + 'VABDCR'
145100030721     C                   ENDIF
145200030721     C                   ENDIF
145300030721     C*** HCR
145400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
145500030721     C                   EVAL      PiStr=SkSplitCSV(i)
145600030721     C                   EXSR      CHKNUM
145700030721     C                   IF        PiInt=*on   AND
145800030721     C                             PiVal<=9999
145900030721     C                   Z-ADD     PiVal         VABHCR
146000030721     C                   ELSE
146100030721     C                   SETON                                        32
146200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146300030721     C                             + ' ' + 'VABHCR'
146400030721     C                   ENDIF
146500030721     C                   ENDIF
146600030721     C*** VMD
146700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
146800030721     C                   EVAL      PiStr=SkSplitCSV(i)
146900030721     C                   EXSR      CHKNUM
147000030721     C                   IF        PiNum=*on
147100030721     C                   Z-ADD     PiVal         VABVMD
147200030721     C                   ELSE
147300030721     C                   SETON                                        32
147400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147500030721     C                             + ' ' + 'VABVMD'
147600030721     C                   ENDIF
147700030721     C                   ENDIF
147800030721     C*** ANT
147900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
148000030721     C                   EVAL      PiStr=SkSplitCSV(i)
148100030721     C                   EXSR      CHKNUM
148200030721     C                   IF        PiInt=*on        AND
148300030721     C                             PiVal<=999999999
148400030721     C                   Z-ADD     PiVal         VABANT
148500030721     C                   ELSE
148600030721     C                   SETON                                        32
148700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
148800030721     C                             + ' ' + 'VABANT'
148900030721     C                   ENDIF
149000030721     C                   ENDIF
149100030721     C*** CMO
149200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
149300030721     C                   EVAL      PiStr=SkSplitCSV(i)
149400030721     C                   EXSR      CHKNUM
149500030721     C                   IF        PiInt=*on
149600030721     C                   Z-ADD     PiVal         Num5_0
149700030721     C                   MOVEL(p)  Num5_0        VABCMO
149800030721     C                   ELSE
149900030721     C                   SETON                                        32
150000030721     C                   EVAL      VABCMO = *zeros
150100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
150200030721     C                             + ' ' + 'VABCMO'
150300030721     C                   ENDIF
150400030721     C                   ENDIF
150500040510     C*
150600040510     C* Reperisco i campi del FIVAT
150700040510     C*** VATNOT
150800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
150900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
151000040510     C                   ENDIF
151100040510     C*** VATTRC
151200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
151300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
151400040510     C                   ENDIF
151500060331     C*** VATNOT_A
151600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
151700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
151800060331     C                   ENDIF
151900060331     C*** VATNOT_B
152000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
152100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
152200060331     C                   ENDIF
152300060331     C*** VATNOT_E
152400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
152500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
152600060331     C                   ENDIF
152700110912     C*** VATNOT_I
152800110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
152900110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
153000110912     C                   ENDIF
153100110912     C*** VATNOT_J
153200110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
153300110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
153400110912     C                   ENDIF
153500110912     C*** VATNOT_4
153600110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
153700110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
153800110912     C                   ENDIF
153900131018     C*** VATNOT_S
154000131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
154100131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
154200131018     C                   ENDIF
154300141128     C*** VATNOT_Y
154400141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
154500141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
154600141128     C                   ENDIF
154700080506     C*
154800080506     C*****************************
154900080506     C* FORZATURE DENTRO CICLO
155000080506     C*****************************
155100080506     C*
155200080506     C* NSP => Stacco un numeratore da AZNUM
155300080506     C                   clear                   TRUL33DS
155400080506     C                   eval      I33OPE = *zeros
155500080506     C                   eval      I33CNU = 302
155600080506     C                   eval      I33NUM = 1
155700080506     C                   movel     TRUL33DS      KPJBU
155800080506     C                   call      'TRUL33R'
155900080506     C                   parm                    KPJBA
156000080506     C                   movel     KPJBU         TRUL33DS
156100080506     C                   if        O33ERR = *zeros
156200080506     C                   z-add     O33NRF        VABNSP
156300080506     C                   z-add     O33NRF        VATNSP
156400080506     C                   else
156500080506     C                   seton                                        31
156600080506     C                   EVAL      vinmsg = %trimr(vinmsg)
156700080506     C                             + ' ' + 'VABNSP VATNSP'
156800080506     C                   endif
156900080506     C*****************************
157000080506     C* FINE FORZATURE DENTRO CICLO
157100080506     C*****************************
157200030715     C                   ENDIF
157300030715     C                   EVAL      i = i + 1
157400030715     C                   ENDDO
157500040510     C*
157600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
157700040510     C                   IF        VABNSP = *zeros
157800040510     C                   SETON                                        31
157900040510     C                   ENDIF
158000040715     C*
158100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
158200040715     C                   IF        VABLNP = *zeros
158300040715     C                   SETON                                        31
158400040715     C                   ENDIF
158500040510     C*
158600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
158700040510     C                   IF        VABRMN = *zeros
158800040510     C                   Z-ADD     VABNSP        VABRMN
158900040510     C                   ENDIF
159000040510     C*
159100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
159200040510     C                   IF        VABRMA = *blanks
159300040714     C                   MOVEL     VABRMN        VABRMA
159400040510     C                   ENDIF
159500070705     C*
159600070705     C* Considerazioni particolari
159700070705     C*** NZD
159800070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
159900070705     C                   EVAL      VABNZD = *blanks
160000070705     C                   ENDIF
160100070705     C*** CAD
160200070705     C                   IF        VABNZD = *blanks
160300070705     C                   EVAL      PiStr=VABCAD
160400070705     C                   EXSR      CHKNUM
160500070705     C                   IF        PiInt=*on
160600070705     C                   Z-ADD     PiVal         Num5_0
160700070705     C                   MOVEL(p)  Num5_0        VABCAD
160800070705     C                   ELSE
160900070705     C                   SETON                                        32
161000070705     C                   EVAL      VABCAD = *zeros
161100070705     C                   EVAL      vinmsg = %trimr(vinmsg)
161200070705     C                             + ' ' + 'VABCAD'
161300070705     C                   ENDIF
161400070705     C                   ENDIF
161500040128     C*
161600040510     C* Se provincia nn valorizzata la reperisco
161700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
161800040510     C                   IF        VABPRD = *blanks
161900040128     C                   CLEAR                   TISI95DS
162000040128     C                   EVAL      I95TCN = '3'
162100040128     C                   Z-ADD     datcor        I95DAT
162200040128     C                   EVAL      I95NAR = VABNZD
162300040128     C                   EVAL      I95CAP = VABCAD
162400040128     C                   EVAL      I95LOC = VABLOD
162500040128     C                   CALL      'TISI95R'
162600040128     C                   PARM                    TISI95DS
162700040128     C                   EVAL      VABPRD = O95PRV
162800040510     C                   ENDIF
162900040113     C*
163000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
163100040113     C                   IF        VABCAS > *zeros
163200040119     C                   EVAL      VABVCA = 'EUR'
163300040119     C                   IF        VABCBO = '1'
163400040113     C                   EVAL      VABCBO = '4'
163500040119     C                   ENDIF
163600040119     C                   IF        VABCBO = '2'
163700040119     C                   EVAL      VABCBO = '6'
163800040119     C                   ENDIF
163900040113     C                   ENDIF
164000020204     C*
164100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
164200020204     C                   EXSR      CHKIMPDIV
164300030715     C                   ENDIF
164400020204     C*
164500010607     C                   ENDSR
164600010607     C*----------------------------------------------------*
164700010601
164800020204
164900020204     C*----------------------------------------------------*
165000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
165100020204     C*----------------------------------------------------*
165200020204     C     CHKIMPDIV     BEGSR
165300020204     C*
165400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
165500020204     C                   Z-ADD     *zeros        wrkDec            9 9
165600020204     C*
165700020204     C* Come prima cosa effettuo considerazioni sulla divisa
165800020204     C                   IF        vabIAS > *zeros
165900020204     C                   IF        vabVAS <> 'EUR'
166000020204     C                   EVAL      vabVAS =  'ITL'
166100020204     C                   ENDIF
166200020204     C                   ENDIF
166300020204     C*
166400020204     C                   IF        vabCAS > *zeros
166500020204     C                   IF        vabVCA <> 'EUR'
166600020204     C                   EVAL      vabVCA =  'ITL'
166700020204     C                   ENDIF
166800020204     C                   ENDIF
166900020204     C*
167000020204     C                   IF        vabVMD > *zeros
167100020204     C                   IF        vabVAD <> 'EUR'
167200020204     C                   EVAL      vabVAD =  'ITL'
167300020204     C                   ENDIF
167400020204     C                   ENDIF
167500020204     C*
167600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
167700020204     C                   Z-ADD     vabIAS        wrkDec
167800020204     C                   IF        wrkDec > *zeros
167900020204     C                   IF        vabVAS = 'ITL'
168000020204     C                   EVAL      vabIAS = *zeros
168100020204     C                   ENDIF
168200020204     C                   ENDIF
168300020204     C*
168400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
168500020204     C                   Z-ADD     vabCAS        wrkDec
168600020204     C                   IF        wrkDec > *zeros
168700020204     C                   IF        vabVCA = 'ITL'
168800020204     C                   EVAL      vabCAS = *zeros
168900020204     C                   ENDIF
169000020204     C                   ENDIF
169100020204     C*
169200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
169300020204     C                   Z-ADD     vabVMD        wrkDec
169400020204     C                   IF        wrkDec > *zeros
169500020204     C                   IF        vabVAD = 'ITL'
169600020204     C                   EVAL      vabVMD = *zeros
169700020204     C                   ENDIF
169800020204     C                   ENDIF
169900020204     C*
170000020204     C                   ENDSR
170100020204     C***
170200020204
170300010330
170400010330
170500010330     C*----------------------------------------------------*
170600010330     C*  CONTROLLO NUMERICITA' CAMPI
170700010330     C*----------------------------------------------------*
170800010330     C     CHKNUM        BEGSR
170900010330     C*
171000010606     C                   IF        PiDecChr = *blanks
171100030715     C                   EVAL      PiDecChr = CharNUM
171200010606     C                   ENDIF
171300010606     C*
171400010606     C                   CALL(e)   'ISNUMERIC'
171500010330     C                   PARM                    PiStr            30
171600010606     C                   PARM                    PiDecChr          1
171700010330     C                   PARM      *ZEROS        PiVal            30 9
171800010330     C                   PARM      '0'           PiInt             1
171900010330     C                   PARM      '0'           PiNum             1
172000010330     C                   IF        %error
172100010606     C                   EVAL      PiNum=*off
172200010330     C                   ENDIF
172300010330     C*
172400010330     C                   ENDSR
172500010330     C***
172600010330
172700010601
172800010601
172900010601
173000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
173100010601     C     repfil        BEGSR
173200010601     C*
173300010601     C                   if        invfil = *zeros and
173400010601     C                             depfil > *zeros and
173500010629     C                             (vinflg = *blanks or
173600010629     C                              vinflg = *zeros)
173700010601     C
173800010601     C                   eval      invfil = depfil
173900010601     C                   endif
174000010601     C*
174100010601     C                   if        depfil <> invfil and
174200010601     C                             invfil > *zeros
174300010601     C                   eval      flgMulti = '1'
174400010601     C                   if        vinflg = *blanks
174500010601     C                   add       1             cntNonEl
174600010601     C                   endif
174700010601     C                   endif
174800010601     C*
174900010601     C                   if        vinflg = '2'
175000010601     C                   eval      flgStato = '2'
175100010601     C                   endif
175200010601     C*
175300010601     C                   ENDSR
175400010601     C***
175500010601
175600010601
175700010601
175800010330
175900040119      /TITLE Invio dei dati al punto operativo.
176000040119     C     invio         BEGSR
176100040119     C*
176200040119     C* 1° invio FIVAT
176300040119     C                   reset                   dscmz
176400070704     C                   move      winvcmz       cmzdst
176500040119     C                   eval      cmzfld = 'FIVATWWR'
176600040119     C                   eval      cmzmbd = vlrhdl
176700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
176800040119     C***                if        prmfir = *blanks
176900040119     C                   eval      cmzfla = 'FIVAT00F'
177000040119     C                   eval      cmzmba = 'FIVAT00F'
177100040119     C***                else
177200040119     C***                eval      cmzfla = prmfir
177300040119     C***                eval      cmzmba = prmfir
177400040119     C***                endif
177500040119     C                   eval      cmznrr = *zeros
177600040119     C                   move      §ctrokvt      cmznrr
177700040119     C                   eval      cmzlba = vlrfl1
177800040119     C                   call(e)   'TIS711C'
177900040119     C                   parm                    dscmz
178000040119     C                   parm      *blanks       esito
178100040119     C                   if        %error
178200040119     C                             or cmzerr = '1'
178300040119     C                             or esito  = '1'
178400040119     C                   eval      wrkesito = '3'
178500040119     C                   else
178600040119     C*
178700040119     C* 2° invio FIVAB
178800040119     C                   reset                   dscmz
178900070704     C                   move      winvcmz       cmzdst
179000040119     C                   eval      cmzfld = vlrfou
179100040119     C                   eval      cmzmbd = vlrhdl
179200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
179300040119     C***                if        prmfir = *blanks
179400040119     C                   eval      cmzfla = 'FIVAB00F'
179500040119     C                   eval      cmzmba = 'FIVAB00F'
179600040119     C***                else
179700040119     C***                eval      cmzfla = prmfir
179800040119     C***                eval      cmzmba = prmfir
179900040119     C***                endif
180000040119     C                   eval      cmznrr = *zeros
180100040119     C                   move      §ctrokvb      cmznrr
180200040119     C                   eval      cmzlba = vlrfl1
180300040119     C                   call(e)   'TIS711C'
180400040119     C                   parm                    dscmz
180500040119     C                   parm      *blanks       esito
180600040119     C                   if        %error
180700040119     C                             or cmzerr = '1'
180800040119     C                             or esito  = '1'
180900040119     C                   eval      wrkesito = '3'
181000040119     C                   endif
181100040119     C                   endif
181200040119     C*
181300040119     C                   ENDSR
181400040119     C***
181500010601
181600010601
181700010601
181800010601
181900010601      /TITLE Invio dei dati al punto operativo.
182000010601     C     opeini        BEGSR
182100010601     C*
182200010601     C* Inizializzo flag e contatori operativi
182300010601     C                   movel     '0'           flgGiro           1
182400010601     C                   movel     '0'           flgMulti          1
182500010601     C                   movel     '1'           flgStato          1
182600010615     C                   movel     '0'           flgOk             1
182700010601     C                   z-add     *zeros        cntNonEl         10 0
182800010601     C                   z-add     *zeros        depfil            3 0
182900010601     C                   z-add     *zeros        invfil            3 0
183000080506     C*
183100080506     C                   eval      wCTM = *blanks
183200080506     C                   eval      wNSR = *blanks
183300080506     C                   eval      wCRC = *blanks
183400080506     C                   eval      wKSC = *blanks
183500080506     C                   eval      wCTR = *blanks
183600010601     C*
183700010601     C                   ENDSR
183800010601     C***
183900070326
184000070326
184100070326
184200070326
184300070326     C     *pssr         BEGSR
184400070329     C*
184500070329     C                   if        %open(tivin00r)
184600070329     C                   close     tivin00r
184700070329     C                   endif
184800070329     C                   if        %open(fivabwwr)
184900070329     C                   close     fivabwwr
185000070329     C                   endif
185100070329     C                   if        %open(fivatwwr)
185200070329     C                   close     fivatwwr
185300070329     C                   endif
185400070326     C*
185500070326     C* Effettuo la chiamata al CLLE preposto
185600070326     C                   call(e)   'TITVVTC'
185700070326     C                   parm                    parccm
185800070326     C                   parm                    parmbr
185900070326     C                   parm      '2'           paropz
186000070326     C*
186100070326     C                   eval      wrkesito = '2'
186200070404     C*
186300070404     C                   seton                                        LR
186400070326     C*
186500070326     C                   ENDSR     '*CANCL'
186600070326     C***
186700080506
186800080506
186900080506
187000080506     C*--------------------------------------------------------
187100080506     C* CARTAB - CARICAMENTO TABELLE                          *
187200080506     C*--------------------------------------------------------
187300080506     C     CARTAB        BEGSR
187400080506     C*
187500080506     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
187600080506     C                   z-add     *zeros        jOSR              3 0
187700080506     C     'OSR'         setll     tntbe01l
187800080506     C                   if        %equal(tntbe01l)
187900080506     C     'OSR'         reade     tntbe01l
188000080506     C                   dow       not %eof(tntbe01l)
188100080506     C                   eval      DOSR = tbeUNI
188200080506     c*
188300080506     c* Verifica e forzatura x nuovi campi aggiunti
188400080506     c                   if        aD§OSRcl6 = *blanks
188500080506     c                   eval      D§OSRcl6 = *zeros
188600080506     c                   endif
188700080506     c                   if        aD§OSRcl7 = *blanks
188800080506     c                   eval      D§OSRcl7 = *zeros
188900080506     c                   endif
189000080506     c                   if        aD§OSRcl8 = *blanks
189100080506     c                   eval      D§OSRcl8 = *zeros
189200080506     c                   endif
189300080506     c                   if        aD§OSRcl9 = *blanks
189400080506     c                   eval      D§OSRcl9 = *zeros
189500080506     c                   endif
189600080506     c                   if        aD§OSRcl10 = *blanks
189700080506     c                   eval      D§OSRcl10 = *zeros
189800080506     c                   endif
189900080506     c                   if        aD§OSRcl11 = *blanks
190000080506     c                   eval      D§OSRcl11 = *zeros
190100080506     c                   endif
190200080506     c                   if        aD§OSRcl12 = *blanks
190300080506     c                   eval      D§OSRcl12 = *zeros
190400080506     c                   endif
190500080506     c                   if        aD§OSRcl13 = *blanks
190600080506     c                   eval      D§OSRcl13 = *zeros
190700080506     c                   endif
190800080506     c                   if        aD§OSRcl14 = *blanks
190900080506     c                   eval      D§OSRcl14 = *zeros
191000080506     c                   endif
191100080506     c                   if        aD§OSRcl15 = *blanks
191200080506     c                   eval      D§OSRcl15 = *zeros
191300080506     c                   endif
191400080506     c                   if        aD§OSRcl16 = *blanks
191500080506     c                   eval      D§OSRcl16 = *zeros
191600080506     c                   endif
191700080506     c*
191800080506     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
191900080506     C                   if        D§OSRCLI <> *zeros
192000080506     C                   add       1             jOSR
192100080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
192200080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
192300080506     C                   movel     D§OSRCLI      wCliOSR
192400080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
192500080506     C                   endif
192600080506     C*
192700080506     C                   if        D§OSRCL2 <> *zeros
192800080506     C                   add       1             jOSR
192900080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
193000080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
193100080506     C                   movel     D§OSRCL2      wCliOSR
193200080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
193300080506     C                   endif
193400080506     C*
193500080506     C                   if        D§OSRCL3 <> *zeros
193600080506     C                   add       1             jOSR
193700080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
193800080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
193900080506     C                   movel     D§OSRCL3      wCliOSR
194000080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
194100080506     C                   endif
194200080506     C*
194300080506     C                   if        D§OSRCL4 <> *zeros
194400080506     C                   add       1             jOSR
194500080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
194600080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
194700080506     C                   movel     D§OSRCL4      wCliOSR
194800080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
194900080506     C                   endif
195000080506     C*
195100080506     C                   if        D§OSRCL5 <> *zeros
195200080506     C                   add       1             jOSR
195300080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
195400080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
195500080506     C                   movel     D§OSRCL5      wCliOSR
195600080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
195700080506     C                   endif
195800080506     C*
195900080506     C                   if        D§OSRCL6 <> *zeros
196000080506     C                   add       1             jOSR
196100080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
196200080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
196300080506     C                   movel     D§OSRCL6      wCliOSR
196400080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
196500080506     C                   endif
196600080506     C*
196700080506     C                   if        D§OSRCL7 <> *zeros
196800080506     C                   add       1             jOSR
196900080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
197000080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
197100080506     C                   movel     D§OSRCL7      wCliOSR
197200080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
197300080506     C                   endif
197400080506     C*
197500080506     C                   if        D§OSRCL8 <> *zeros
197600080506     C                   add       1             jOSR
197700080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
197800080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
197900080506     C                   movel     D§OSRCL8      wCliOSR
198000080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
198100080506     C                   endif
198200080506     C*
198300080506     C                   if        D§OSRCL9 <> *zeros
198400080506     C                   add       1             jOSR
198500080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
198600080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
198700080506     C                   movel     D§OSRCL9      wCliOSR
198800080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
198900080506     C                   endif
199000080506     C*
199100080506     C                   if        D§OSRCL10 <> *zeros
199200080506     C                   add       1             jOSR
199300080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
199400080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
199500080506     C                   movel     D§OSRCL10     wCliOSR
199600080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
199700080506     C                   endif
199800080506     C*
199900080506     C                   if        D§OSRCL11 <> *zeros
200000080506     C                   add       1             jOSR
200100080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
200200080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
200300080506     C                   movel     D§OSRCL11     wCliOSR
200400080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
200500080506     C                   endif
200600080506     C*
200700080506     C                   if        D§OSRCL12 <> *zeros
200800080506     C                   add       1             jOSR
200900080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
201000080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
201100080506     C                   movel     D§OSRCL12     wCliOSR
201200080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
201300080506     C                   endif
201400080506     C*
201500080506     C                   if        D§OSRCL13 <> *zeros
201600080506     C                   add       1             jOSR
201700080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
201800080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
201900080506     C                   movel     D§OSRCL13     wCliOSR
202000080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
202100080506     C                   endif
202200080506     C*
202300080506     C                   if        D§OSRCL14 <> *zeros
202400080506     C                   add       1             jOSR
202500080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
202600080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
202700080506     C                   movel     D§OSRCL14     wCliOSR
202800080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
202900080506     C                   endif
203000080506     C*
203100080506     C                   if        D§OSRCL15 <> *zeros
203200080506     C                   add       1             jOSR
203300080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
203400080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
203500080506     C                   movel     D§OSRCL15     wCliOSR
203600080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
203700080506     C                   endif
203800080506     C*
203900080506     C                   if        D§OSRCL16 <> *zeros
204000080506     C                   add       1             jOSR
204100080506     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
204200080506     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
204300080506     C                   movel     D§OSRCL16     wCliOSR
204400080506     C                   eval      SkCliOSR(jOSR) = DsCliOSR
204500080506     C                   endif
204600080506     C*
204700080506     C     'OSR'         reade     tntbe01l
204800080506     C                   enddo
204900080506     C                   endif
205000080506     C*
205100080506     C                   ENDSR
205200080506     C***
205300070326
205400070326
205500010330
205600010330
205700000613     C     *inzsr        BEGSR
205800990910     C*
205900990910     C     *entry        plist
206000990920     C                   parm                    tivlrds
206100990921     C                   parm      wrkesito      esito
206200000724     C                   parm                    prmlit
206300000710     C                   parm                    prmfir
206400080506     C*
206500080506     C* CARICO LE TABELLE OCCORRENTI
206600080506     C                   exsr      CARTAB
206700010330     C*
206800010330     C* CALCOLA LA DATA CORRENTE
206900010330     C                   time                    wn14             14 0
207000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
207100010330     C                   z-add     wn8           g08dat
207200010330     C                   z-add     *zeros        g08inv
207300010330     C                   movel     '0'           g08err
207400010330     C                   call      'XSRDA8'
207500010330     C                   parm                    wlbda8
207600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
207700000613     C*
207800000613     C                   ENDSR
207900000613     C***
208000990908
208100040122     Otitv92p   E            riepilogo         2
208200990915     O                                              'Upload via Internet'
208300990915     O                                           +1 'Traduzione TIVIN00R -'
208400021025     O                                           +1 'FIVABWWR'
208500990915     O                                           +1 'Testate bolle'
208600990915     O          E            riepilogo   2
208700990915     O                       wrkdata
208800990915     O                       wrkora              +1
208900990915     O                       procname            +1
209000990915     O          E            riepilogo   2
209100990915     O                                              'Cliente..................:'
209200010605     O                       VABCCM        z     +1
209300990915     O          E            riepilogo   2
209400990920     O                                              'Riferimento Strategi.....:'
209500990920     O                       vlrhdl              +1
209600990915     O          E            riepilogo   2
209700990915     O                                              'Giusti...................:'
209800040119     O                       §CTROKVB      2   +  1
209900990915     O          E            riepilogo   2
210000990915     O                                              'Sbagliati e corretti.....:'
210100971022     O                       §CTRMO        2   +  1
210200990915     O          E            riepilogo   2
210300990915     O                                              'Sbagliati e scartati.....:'
210400971022     O                       §CTRNO        2   +  1
210500000613
210600040122     Otitv92ps  E            testdett          2
210700000613     O                                              'Upload via Internet'
210800000613     O                                           +1 'Traduzione TIVIN00R -'
210900021025     O                                           +1 'FIVABWWR'
211000000613     O                                           +1 'Testate bolle'
211100000616     O          E            testdett    3
211200000613     O                                           +2 'N° rec'
211300000613     O                                           +3 'Anteprima contenuto'
211400000616     O          E            rigadett    2
211500000613     O                       rrnum               +2
211600000621     O                       recko               +3
211700000616     O          E            findett     2
211800000613     O                       wrkdata
211900000613     O                       wrkora              +1
212000000613     O                       procname            +1
212100000616     O          E            findett     2
212200000613     O                                              'Cliente..................:'
212300010605     O                       VABCCM        z     +1
212400000616     O          E            findett     2
212500000613     O                                              'Riferimento Strategi.....:'
212600000613     O                       vlrhdl              +1
212700000616     O          E            findett     2
212800000613     O                                              'Giusti...................:'
212900040119     O                       §CTROKVB      2   +  1
213000000616     O          E            findett     2
213100000613     O                                              'Sbagliati e corretti.....:'
213200000613     O                       §CTRMO        2   +  1
213300000616     O          E            findett     2
213400000613     O                                              'Sbagliati e scartati.....:'
213500000613     O                       §CTRNO        2   +  1
213600000613** CMD - COMANDI CL
213700040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
213800040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
213900040122DLTOVR FILE(TITV92P TITV92PS) LVL(*)
214000010606
214100000613
214200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
214300040510
214400040122
214500040113
