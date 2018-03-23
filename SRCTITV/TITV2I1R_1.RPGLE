000100071120      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600071120     FEDIVABwr  O    E             DISK    usropn
000700071120     FEDIVATwr  O    E             DISK    usropn
000800040122     Ftitv92p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100040122     Ftitv92ps  O    f  198        PRINTER usropn
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
002200040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300090403     D CharSOS         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800060331     D wVATNOT_A       s                   LIKE(VATNOT)
005900060331     D wVATNOT_B       s                   LIKE(VATNOT)
006000060331     D wVATNOT_E       s                   LIKE(VATNOT)
006100110912     D wVATNOT_I       s                   LIKE(VATNOT)
006200110912     D wVATNOT_J       s                   LIKE(VATNOT)
006300110912     D wVATNOT_4       s                   LIKE(VATNOT)
006400060331     D wVABNSP         s                   LIKE(VABNSP) INZ
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200010330     D*------------------
007300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007400010330     D*------------------
007500010330     D WLBDA8          DS                  INZ
007600010330     D  G08DAT                 1      8  0
007700010330     D  G08INV                 9     16  0
007800010330     D  G08ERR                17     17
007900010330     D  G08TGI                18     22  0
008000031201     D*------------------
008100031201     D* Costanti
008200031201     D*------------------
008300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500010330
008600010330
008700010330
008800990915     C                   time                    wrkdata
008900990915     C                   time                    wrkora
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300010601     C*
009400010601     C                   exsr      opeini
009500010605     C                   exsr      rwvab
009600010601     C*
009700010601     C                   seton                                        lr
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700010601     C                   if        invfil <> *zeros and
010800010601     C                             flgGiro = '0'
010900010601     C*
011000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010601     C                   eval      flgGiro = '1'
011200000724     C*
011300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500010601     C     invfil        chain     azorg01l
011600000724     C                   if        %found
011700000724     C                   movel(p)  CMD(1)        depcmd
011800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011900000724     C*
012000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000724     C                   PARM                    depcmd
012400000724     C                   PARM                    LENGH
012500000724     C*
012600000724     C                   endif
012700000616     C*
012800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900000616     C                   movel(p)  CMD(2)        depcmd
013000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013100000616     C*
013200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    depcmd
013600000616     C                   PARM                    LENGH
013700000613     C*
013800040122     C                   if        not %open(titv92ps)
013900040122     C                   open      titv92ps
014000070329     C                   except    testdett
014100000613     C                   endif
014200010601     C*
014300010601     C                   endif
014400010601     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010601
015000010601
015100010601     C*--------------------------------------------------------
015200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015300010601     C*--------------------------------------------------------
015400010601     C     ENDELA        BEGSR
015500990915     C*
015600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015700040119     C*
015800040119     C* Effettuo la chiamata al CLLE preposto
015900071121     C                   call(e)   'TITVEVTC'
016000040510     C                   parm                    parccm
016100040119     C                   parm                    parmbr
016200040119     C                   parm      '2'           paropz
016300050201     C*
016400050201     C* Effettuo lancio TISI95 solo x chiusura
016500050201     C                   CLEAR                   TISI95DS
016600050201     C                   EVAL      I95TLA = 'C'
016700050201     C                   CALL      'TISI95R'
016800050201     C                   PARM                    TISI95DS
016900000613     C*
017000040122     C                   if        %open(titv92ps)
017100000616     C                   except    findett
017200040122     C                   close     titv92ps
017300000613     C                   endif
017400000616     C*
017500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017600011113     C                   Z-ADD     150           LENGH            15 5
017700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017800000616     C                   PARM                    CMD(3)
017900000616     C                   PARM                    LENGH
018000000616     C*
018100010601     C                   ENDSR
018200010601     C***
018300010601
018400010601
018500010601
018600000613
018700000613     C*--------------------------------------------------------
018800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018900000613     C*--------------------------------------------------------
019000000621     C     STPR          BEGSR
019100000613     C*
019200040122     C                   if        not %open(titv92p)
019300040122     C   50              open      titv92p
019400990915     C                   endif
019500990915     C*
019600030715     C   50              except    riepilogo
019700990915     C*
019800040122     C                   if        %open(titv92p)
019900040122     C   50              close     titv92p
020000990914     C                   endif
020100000613     C*
020200000613     C                   ENDSR
020300000613     C***
020400000613
020500010601
020600010601
020700010330     C*--------------------------------------------------------
020800071120     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
020900010330     C*--------------------------------------------------------
021000010605     C     RWVAB         BEGSR
021100010330     C*
021200010330     C                   if        not %open(tivin00r)
021300010330     C                   open      tivin00r
021400010330     C                   endif
021500071120     C                   if        not %open(edivabwr)
021600071120     C                   open      edivabwr
021700010330     C                   endif
021800040119     C*
021900071120     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022000040119     C                   exsr      prevat
022100040119     C*
022200040119     C                   if        chkcall = '0'
022300040119     C*
022400071120     C                   if        not %open(edivatwr)
022500071120     C                   open      edivatwr
022600040119     C                   endif
022700010330     C*
022800040119     C                   clear                   §CTROKVB          7 0
022900040119     C                   clear                   §CTROKVT          7 0
023000040119     C                   clear                   §CTRMO            7 0
023100040119     C                   clear                   §CTRNO            7 0
023200010330     C*
023300030822     C                   do        *HIVAL
023400010330     C*
023500030822     C                   read      tivin00r                               70
023600010618     C*
023700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023800010618     C                   if        vinflg = '1'
023900010618     C                   eval      flgOk = '1'
024000010618     C                   endif
024100040510     C*
024200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024300040510     C                   z-add     *zeros        wLenVINDTA        4 0
024400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024700090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024800040510     C                   z-add     1             wFlgVINDTA
024900040510     C                   leave
025000040510     C                   endif
025100040510     C                   enddo
025200010618     C*
025300040510     C                   if        vindta > *blanks AND
025400040510     C                             wFlgVINDTA = 1
025500010330     C                   add       1             rrnum
025600010330     C*
025700010601     C                   if        *in70 = *off and
025800010330     C                             (vinflg = *blanks
025900010330     C                              or vinflg = '0'
026000010330     C                              or vinflg = '2')
026100010330     C*
026200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026300010711     C                   if        vinflg = *blanks or vinflg = '0'
026400010711     C                   clear                   vinmsg
026500010711     C                   endif
026600010601     C*
026700010330     C                   exsr      inzvar
026800090403     C*
026900090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
027000090403     C* separatori campi
027100090403     C                   if        CharTXT <> *blanks
027200090403     C                   z-add     *zeros        wLenVINDTA        4 0
027300090403     C                   z-add     *zeros        wFlgVINDTA        1 0
027400090403     C                   setoff                                       80
027500090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
027600090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
027700090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
027800090403     C                   if        not *in80
027900090403     C                   seton                                        80
028000090403     C                   else
028100090403     C                   setoff                                       80
028200090403     C                   endif
028300090403     C                   endif
028400090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
028500090403     C                             and *in80 = *on
028600090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
028700090403     C                   endif
028800090403     C                   enddo
028900090403     C                   endif
029000090403     C*
029100010605     C                   exsr      impvab
029200010601     C*
029300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029400010605     C                   eval      depfil = VABLNP
029500010601     C                   exsr      repfil
029600010601     C                   if        depfil = invfil
029700021025     C                   if        vlrpoi = 999
029800030822     C                   move(P)   invfil        VABFGS
029900070704     C                   move(P)   invfil        winvcmz           3
030000021025     C                   else
030100030822     C                   move(P)   vlrpoi        VABFGS
030200070704     C                   move(P)   vlrpoi        winvcmz
030300021025     C                   endif
030400010601     C*
030500010601     C                   exsr      PREELA
030600010601     C*
030700010604     C* Ebbene...
030800010604     C*
030900040119     C  N31              add       1             §CTROKVB
031000040119     C   32              add       1             §CTRMO
031100040119     C   31              add       1             §CTRNO
031200030822     C                   if        wGiro = 2
031300040510     C* Scrivo sempre il VAT
031400040510     C  N31              exsr      wrivat
031500040510     C* Scrivo il VAB solo a rottura d spedizione
031600040119     C                   if        depspe =  *zeros OR
031700040119     C                             depspe <> vabNSP
031800040119     C  N31              exsr      wrivab
031900040119     C  N31              eval      depspe = vabNSP
032000040119     C                   endif
032100040119     C                   endif
032200010604     C*
032300010604     C                   if        *in31 = *off and
032400010604     C                             *in32 = *off
032500010604     C                   eval      vinflg = '1'
032600010604     C                   else
032700010604     C                   eval      recko = vindta
032800010604     C                   except    rigadett
032900010604     C                   eval      vinflg = '2'
033000010604     C                   endif
033100010604     C                   endif
033200010601     C*
033300010604     C                   endif
033400010604     C*
033500010330     C                   else
033600010330     C                   eval      vinflg = '1'
033700010330     C                   endif
033800010601     C*
033900010601     C  N70              update    tivin000
034000010330     C*
034100030822     C  N70              enddo
034200040119     C*
034300040119     C                   endif                                                  (endif - chkcall)
034400010601     C*
034500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034600010601     C                   if        cntNonEl = *zeros or
034700010601     C                             flgMulti = '0'
034800010330     C* Se non ci sono record con errori ...
034900010601     C                   if        §ctrno = 0 and
035000010604     C                             §ctrmo = 0 and
035100010601     C                             flgStato <> '2'
035200010330     C* ... restituisco esito OK.
035300010330     C                   eval      wrkesito = '0'
035400010330     C                   else
035500040119     C                   if        §ctrokvb > 0
035600010330     C                   eval      wrkesito = '1'
035700010330     C                   else
035800010615     C                   if        flgOk = '0'
035900010615     C                   eval      wrkesito = '2'
036000010615     C                   else
036100010615     C                   eval      wrkesito = '6'
036200010615     C                   endif
036300010330     C                   endif
036400010330     C                   endif
036500010601     C                   else
036600010601     C                   eval      wrkesito = '9'
036700010601     C                   endif
036800010330     C*
036900010330     C                   if        %open(tivin00r)
037000010330     C                   close     tivin00r
037100010330     C                   endif
037200071120     C                   if        %open(edivabwr)
037300071120     C                   close     edivabwr
037400010330     C                   endif
037500071120     C                   if        %open(edivatwr)
037600071120     C                   close     edivatwr
037700040512     C                   endif
037800010601     C*
037900010601     C                   if        vlrpoi <> 999
038000010601     C                   eval      invfil = vlrpoi
038100010601     C                   endif
038200010330     C*
038300040119     C                   if        §ctrokvb > 0
038400010601     C                             and invfil > *zeros
038500010330     C                   exsr      invio
038600010330     C                   endif
038700010601     C*
038800010618     C                   if        flgGiro = '1'
038900010601     C                   exsr      endela
039000010618     C                   endif
039100010330     C*
039200010330     C                   ENDSR
039300010330     C***
039400071120
039500071120
039600071120
039700071120     C*----------------------------------------------------*
039800071120     C*  VALORIZZAZIONE CAMPI CMR
039900071120     C*----------------------------------------------------*
040000071120     C     VALCMR        BEGSR
040100071120     C*
040200071120     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
040300071120     C                   EVAL      VABCMR = %char(DATCOR)+%char(ORACOR)
040400071120     C                   EVAL      VABDCM = DATCOR
040500071120     C                   EVAL      VABDTS = DATCOR
040600071120     C                   EVAL      VABHMS = ORACOR
040700071120     C                   EVAL      VABCNT = 1
040800071120     C*
040900071120     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
041000071120     C                   EVAL      VATCMR = %char(DATCOR)+%char(ORACOR)
041100071120     C                   EVAL      VATCNT = 1
041200071120     C*
041300071120     C                   ENDSR
041400071120
041500071120
041600071120
041700040119
041800040119
041900040119
042000040119     C*----------------------------------------------------*
042100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
042200040119     C*----------------------------------------------------*
042300040510     C     WRIVAB        BEGSR
042400040119     C*
042500060331     C* Gestisco l'eventuale rottura x numero spedizione
042600060331     C                   if        VABNSP <> wVABNSP
042700070301     C                   if        wVATNOT_E <> *blanks
042800070301     C                   eval      VABCTM = '7Q'
042900070301     C                   endif
043000071120     C                   exsr      VALCMR
043100071120     C                   write     edivab00                                     => scarico il VAB
043200060331     C                   eval      wVABNSP = VABNSP
043300060331     C                   endif
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
044800060512     C                   eval      VATNSP = VABNSP
044900060512     C                   eval      VATLNP = VABLNP
045000040714     C*
045100040714     C* Solo se VATNOT valorizzato scarico il buffer
045200040714     C                   if        VATNOT <> *blanks
045300040119     C*
045400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
045500040510     C                   if        VATTRC = *blanks
045600040510     C                   eval      VATTRC = 'E'
045700040510     C                   endif
045800071120     C                   exsr      VALCMR
045900071120     C                   write     EDIVAT00
046000040119     C                   add       1             §CTROKVT
046100040714     C*
046200040714     C                   endif
046300060331     C*
046400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
046500060331     C                   if        wVATNOT_A <> *blanks
046600060331     C                   eval      VATNOT = wVATNOT_A
046700060331     C                   eval      VATTRC = 'A'
046800071120     C                   exsr      VALCMR
046900071120     C                   write     EDIVAT00
047000060331     C                   add       1             §CTROKVT
047100060331     C                   endif
047200060331     C*
047300060331     C                   if        wVATNOT_B <> *blanks
047400060331     C                   eval      VATNOT = wVATNOT_B
047500060331     C                   eval      VATTRC = 'B'
047600071120     C                   exsr      VALCMR
047700071120     C                   write     EDIVAT00
047800060331     C                   add       1             §CTROKVT
047900060331     C                   endif
048000060331     C*
048100060331     C                   if        wVATNOT_E <> *blanks
048200060331     C                   eval      VATNOT = wVATNOT_E
048300060331     C                   eval      VATTRC = 'E'
048400071120     C                   exsr      VALCMR
048500071120     C                   write     EDIVAT00
048600060331     C                   add       1             §CTROKVT
048700060331     C                   endif
048800110912     C*
048900110912     C                   if        wVATNOT_I <> *blanks
049000110912     C                   eval      VATNOT = wVATNOT_I
049100110912     C                   eval      VATTRC = 'I'
049200110912     C                   exsr      VALCMR
049300110912     C                   write     EDIVAT00
049400110912     C                   add       1             §CTROKVT
049500110912     C                   endif
049600110912     C*
049700110912     C                   if        wVATNOT_J <> *blanks
049800110912     C                   eval      VATNOT = wVATNOT_J
049900110912     C                   eval      VATTRC = 'J'
050000110912     C                   exsr      VALCMR
050100110912     C                   write     EDIVAT00
050200110912     C                   add       1             §CTROKVT
050300110912     C                   endif
050400110912     C*
050500110912     C                   if        wVATNOT_4 <> *blanks
050600110912     C                   eval      VATNOT = wVATNOT_4
050700110912     C                   eval      VATTRC = '4'
050800110912     C                   exsr      VALCMR
050900110912     C                   write     EDIVAT00
051000110912     C                   add       1             §CTROKVT
051100110912     C                   endif
051200040119     C*
051300040119     C                   ENDSR
051400040119
051500040119
051600040119
051700040119     C*----------------------------------------------------*
051800071120     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051900040119     C*----------------------------------------------------*
052000040119     C     PREVAT        BEGSR
052100040119     C*
052200071120     C* Compongo il nome del membro da dare al EDIVATWR
052300040119     C                   eval      parmbr = vlrhdl
052400040119     C                   movel     'M'           parmbr
052500040510     C                   eval      parccm = vlrksc
052600040119     C                   eval      paropz = '1'
052700040119     C* Effettuo la chiamata al CLLE preposto
052800071121     C                   call(e)   'TITVEVTC'
052900040119     C                   parm                    parccm
053000040119     C                   parm                    parmbr
053100040119     C                   parm                    paropz
053200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053300040119     C                   if        %error
053400040119     C                   movel     '1'           chkcall
053500040119     C                   else
053600040119     C                   movel     '0'           chkcall
053700040119     C                   endif
053800040119     C*
053900040119     C                   ENDSR
054000010601
054100010601
054200010601
054300010330     C*----------------------------------------------------*
054400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054500010330     C*----------------------------------------------------*
054600040119     C     INZVAR        BEGSR
054700010330     C*
054800030822     C* Inizializzo variabili di wrk
054900020204     C                   Z-ADD     *zeros        Num5_0            5 0
055000090403     C                   z-add     *zeros        invfil
055100060331     C                   MOVEL     *blanks       wVATNOT_A
055200060331     C                   MOVEL     *blanks       wVATNOT_B
055300060331     C                   MOVEL     *blanks       wVATNOT_E
055400110912     C                   MOVEL     *blanks       wVATNOT_I
055500110912     C                   MOVEL     *blanks       wVATNOT_J
055600110912     C                   MOVEL     *blanks       wVATNOT_4
055700030822     C*
055800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055900071120     C                   CLEAR                   EDIVAB00
056000071120     C                   CLEAR                   EDIVAT00
056100030822     C                   CLEAR                   SkSplitCSV
056200030822     C                   EVAL      i = 1
056300030822     C                   EVAL      posDa = *zeros
056400030822     C                   EVAL      posA  = *zeros
056500030822     C*
056600030822     C* Reimposto i valori di default
056700030822     C                   EXSR      DEFCAM
056800030822     C*
056900010330     C                   ENDSR
057000010330     C*----------------------------------------------------*
057100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057200010330     C*----------------------------------------------------*
057300010330     C     DEFCAM        BEGSR
057400030822     C*
057500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057600030715     C* e delimitatore testo.
057700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058000090403     C*
058100090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058200090403     C                   EVAL      CharSOS = CharNUM
058300030822     C*
058400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058600030715     C                   SETON                                        50
058700030715     C                   ELSE
058800030715     C                   SETOFF                                       50
058900030715     C                   ENDIF
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400030822     C* NCL
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'NCL'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABNCL
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* CCM
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'CCM'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABCCM
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* LNP
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'LNP'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABLNP
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* NRS
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'NRS'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABNRS
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CTR
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CTR'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABCTR
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* PKB
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'PKB'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900030822     C                   Z-ADD     PiVal         VABPKB
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* VLB
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VLB'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200030822     C                   Z-ADD     PiVal         VABVLB
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* QFT
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'QFT'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABQFT
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* CBO
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CBO'
072300030822     C                   EVAL      VABCBO=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TSP
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TSP'
073200030822     C                   EVAL      VABTSP=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAS
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAS'
074100030822     C                   EVAL      VABVAS=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VCA'
075000030822     C                   EVAL      VABVCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GCA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GCA'
075900030822     C                   EVAL      VABGCA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* CTM
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'CTM'
076800030822     C                   EVAL      VABCTM=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* FFD
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'FFD'
077700030822     C                   EVAL      VABFFD=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* VAD
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'VAD'
078600030822     C                   EVAL      VABVAD=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GMA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GMA'
079500030822     C                   EVAL      VABGMA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GGA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GGA'
080400030822     C                   EVAL      VABGGA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GVA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GVA'
081300030822     C                   EVAL      VABGVA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* TC1
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'TC1'
082200030822     C                   EVAL      VABTC1=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* TC2
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'TC2'
083100030822     C                   EVAL      VABTC2=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500080516     C* TIC
083600080516     C                   IF        %subst(
083700080516     C                             %subst(vlrppt:posDaDft+1:
083800080516     C                             posADft-posDaDft-1):1:3)
083900080516     C                             = 'TIC'
084000080516     C                   EVAL      VABTIC=%trim(%subst(
084100080516     C                             %subst(vlrppt:posDaDft+1:
084200080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300080516     C                   ENDIF
084400040714     C* VATTRC
084500040714     C                   IF        %subst(
084600040714     C                             %subst(vlrppt:posDaDft+1:
084700040714     C                             posADft-posDaDft-1):1:3)
084800040714     C                             = 'TRC'
084900040714     C                   EVAL      VATTRC=%trim(%subst(
085000040714     C                             %subst(vlrppt:posDaDft+1:
085100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200040714     C                   ENDIF
085300030822     C* ...
085400030822     C                   ENDIF
085500030822     C                   ENDDO
085600020204     C*
085700010330     C                   ENDSR
085800010607     C*----------------------------------------------------*
085900071120     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
086000010607     C*----------------------------------------------------*
086100010607     C     IMPVAB        BEGSR
086200010607     C*
086300010607     C                   SETOFF                                       3132
086400030515     C*
086500010607     C                   MOVEL     datcor        VABAAS
086600010607     C                   MOVE      datcor        VABMGS
086700010607     C*
086800030715     C**********
086900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087000030715     C**********
087100030715     C                   IF        wGiro = *zeros
087200040113     C*
087300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087500040510     C                             %trim(intcol(3))<>*blanks
087600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087700040113     C                                      %trim(intcol(3))
087800040510     C                   ENDIF
087900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088000031201     C     minu:maiu     XLATE     vindta        vindta
088100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088200030715     C                   DOW       posDa <= %len(%trim(vindta))
088300030715     C*
088400030715     C* Gestisco il 1° campo
088500030715     C                   IF        i = 1
088600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088700030715     C                   EVAL      posDa = 1
088800030715     C* Eseguo lo scan x trovare la fine del primo campo
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C* X i campi successivi al 1°
089400030715     C                   ELSE
089500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089600030715     C                   EVAL      posDa = posA + 1
089700030715     C* Eseguo lo scan x trovare la fine del campo corrente
089800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089900030715     C* Gestisco l'ultimo campo
090000030715     C                   IF        posA = *zeros
090100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C                   LEAVE
090600030715     C                   ELSE
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   ENDIF
091100030715     C                   ENDIF
091200030715     C* Incremento il contatore d campo
091300030715     C                   EVAL      i = i +1
091400030715     C                   ENDDO
091500030715     C*
091600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091700030715     C* delimitazione testo
091800030715     C                   EVAL      i = 1
091900030715     C                   DOW       i < %elem(SkSplitFLD)
092000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092200030715     C                   EVAL      i = i + 1
092300030715     C                   ENDDO
092400030715     C                   EVAL      wGiro = 1
092500030715     C                   ELSE
092600030715     C                   EVAL      wGiro = 2
092700031204     C**********
092800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092900031204     C* d separatore campo
093000031204     C**********
093100031204     C                   EVAL      vindta = %trim(vindta)
093200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093300031204     C                   EVAL      vindta = %subst(vindta:2)
093400031204     C                   ENDDO
093500031204     C*
093600031204     C                   Z-ADD     *zeros        lunghInput        4 0
093700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094100031204     C                   ENDDO
094200030715     C**********
094300030715     C* Effettuo lo split del campo dati d input
094400030715     C**********
094500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094600030715     C                   DOW       posDa <= %len(%trim(vindta))
094700030715     C*
094800030715     C* Gestisco il 1° campo
094900030715     C                   IF        i = 1
095000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095100030715     C                   EVAL      posDa = 1
095200030715     C* Eseguo lo scan x trovare la fine del primo campo
095300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095400030715     C* A questo punto "estraggo" il campo corrente
095500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095600030715     C                                                    (posA-posDa))
095700030715     C* X i campi successivi al 1°
095800030715     C                   ELSE
095900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096000030715     C                   EVAL      posDa = posA + 1
096100031204     C* Verifico che nn vi sia il campo nn valorizzato
096200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096300031204     C* Se campo nn valorizzato skippo al prossimo
096400031204     C                   EVAL      SkSplitCSV(i) = *blanks
096500031204     C                   EVAL      posA  = posA + 1
096600031204     C                   ELSE
096700030715     C* Eseguo lo scan x trovare la fine del campo corrente
096800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096900030715     C* Gestisco l'ultimo campo
097000030715     C                   IF        posA = *zeros
097100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097200030715     C* A questo punto "estraggo" il campo corrente
097300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097400030715     C                                                    (posA-posDa))
097500030715     C                   LEAVE
097600030715     C                   ELSE
097700030715     C* A questo punto "estraggo" il campo corrente
097800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097900030715     C                                                    (posA-posDa))
098000030715     C                   ENDIF
098100031204     C                   ENDIF
098200030715     C                   ENDIF
098300030715     C* Incremento il contatore d campo
098400030715     C                   EVAL      i = i +1
098500030715     C                   ENDDO
098600030715     C*
098700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098800030715     C* delimitazione testo
098900030715     C                   EVAL      i = 1
099000030715     C                   DOW       i < %elem(SkSplitCSV)
099100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099300030715     C                   EVAL      i = i + 1
099400030715     C                   ENDDO
099500030715     C*
099600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099700030715     C                   EVAL      i = 1
099800030715     C                   DOW       i < %elem(SkSplitFLD)
099900030715     C*
100000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100200030715     C                             SkSplitCSV(i) <> *blanks
100300030721     C*** ATB
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
100500030721     C                   EVAL      VABATB = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** RSD
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101200030715     C* ==
101300030715     C                   ENDIF
101400030721     C*** RD2
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030715     C*** IND
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102000030715     C                   EVAL      VABIND = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** LOD
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
102400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** PRD
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
102800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030721     C*** NZD
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030715     C*** RMA
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030715     C*** NOT
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104100030715     C                   ENDIF
104200030715     C*** NT2
104300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
104400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
104500030715     C                   ENDIF
104600030715     C*** VCA
104700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
104800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104900030715     C                   ENDIF
105000030715     C*** VAS
105100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105300030715     C                   ENDIF
105400030715     C*** FFD
105500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105700030715     C                   ENDIF
105800030721     C*** GC1
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106100100304     C                   IF        %subst(VABGC1:1) = 'P' or
106200100304     C                             %subst(VABGC1:1) = 'M'
106300100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
106400100304     C                   ENDIF
106500030721     C                   ENDIF
106600030721     C*** GC2
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
106800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
106900100304     C                   IF        %subst(VABGC2:1) = 'P' or
107000100304     C                             %subst(VABGC2:1) = 'M'
107100100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
107200100304     C                   ENDIF
107300030721     C                   ENDIF
107400030721     C*** TSP
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107700030721     C                   ENDIF
107800040510     C*** CBO
107900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108100040510     C                   ENDIF
108200030721     C*** NAS
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** TIC
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GCA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** XCO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** CTM
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TCR
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** CTS
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** FTM
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** VAD
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** GMA
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*** GGA
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112500030721     C                   ENDIF
112600030721     C*** GVA
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112900030721     C                   ENDIF
113000030721     C*** TC1
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113300030721     C                   ENDIF
113400030721     C*** TC2
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113700030721     C                   ENDIF
113800030721     C*** SCL
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114100030721     C                   ENDIF
114200030721     C*** RMO
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114500030721     C                   ENDIF
114600030721     C*** NMO
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114900030721     C                   ENDIF
115000030721     C*
115100030721     C* Reperisco quindi i campi numerici...
115200040510     C*** NSP
115300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115400040510     C                   EVAL      PiStr=SkSplitCSV(i)
115500040510     C                   EXSR      CHKNUM
115600040510     C                   IF        PiInt=*on
115700040510     C                   Z-ADD     PiVal         VABNSP
115800040510     C                   ELSE
115900040510     C                   SETON                                        31
116000040510     C                   EVAL      VABNSP = *zeros
116100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116200040510     C                             + ' ' + 'VABNSP'
116300040510     C                   ENDIF
116400040510     C                   ENDIF
116500070703     C*** RMN
116600070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116700070703     C                   EVAL      PiStr=SkSplitCSV(i)
116800070703     C                   EXSR      CHKNUM
116900070703     C                   IF        PiInt=*on
117000070703     C                   Z-ADD     PiVal         VABRMN
117100070703     C                   ELSE
117200070703     C                   SETON                                        32
117300070703     C                   EVAL      vinmsg = %trimr(vinmsg)
117400070703     C                             + ' ' + 'VABRMN'
117500070703     C                   ENDIF
117600070703     C                   ENDIF
117700070705     C*** CAD
117800070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117900070705     C                   EVAL      VABCAD = SkSplitCSV(i)
118000070705     C                   ENDIF
118100030721     C*** NCL
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on
118600030721     C                   Z-ADD     PiVal         VABNCL
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABNCL = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100040122     C                             + ' ' + 'VABNCL'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** PKB
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiNum=*on
119900030721     C                   Z-ADD     PiVal         VABPKB
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      VABPKB = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABPKB'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** CAS
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200030721     C                   Z-ADD     PiVal         VABCAS
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      VABCAS = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABCAS'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** CCM
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on      AND
122500030721     C                             PiVal<=9999999 AND
122600030721     C                             PiVal>*zeros
122700030721     C                   Z-ADD     PiVal         VABCCM
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      VABCCM = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200040510     C                             + ' ' + 'VABCCM'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** LNP
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on    AND
124000030721     C                             PiVal<=999   AND
124100030721     C                             PiVal>*zeros
124200030721     C                   Z-ADD     PiVal         VABLNP
124300030721     C                   ELSE
124400040715     C                   SETON                                        32
124500030721     C                   EVAL      VABLNP = *zeros
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700040510     C                             + ' ' + 'VABLNP'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** NRS
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on AND
125500030721     C                             PiVal<=99
125600030721     C                   Z-ADD     PiVal         VABNRS
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABNRS'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** LNA
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on    AND
126800030721     C                             PiVal<=999   AND
126900030721     C                             PiVal>*zeros
127000030721     C                   Z-ADD     PiVal         VABLNA
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      VABLNA = *zeros
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABLNA'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** CTR
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on  AND
128300030721     C                             PiVal<=999
128400030721     C                   Z-ADD     PiVal         VABCTR
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABCTR'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** IAS
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiNum=*on
129600030721     C                   Z-ADD     PiVal         VABIAS
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABIAS'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** VLB
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiNum=*on
130800030721     C                   Z-ADD     PiVal         VABVLB
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABVLB'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** QFT
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiNum=*on
132000030721     C                   Z-ADD     PiVal         VABQFT
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABQFT'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** NCD
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on      AND
133200030721     C                             PiVal<=9999999
133300030721     C                   Z-ADD     PiVal         VABNCD
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABNCD'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** NCA
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on      AND
134500030721     C                             PiVal<=9999999
134600030721     C                   Z-ADD     PiVal         VABNCA
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABNCA'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** ZNC
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on AND
135800030721     C                             PiVal<=99
135900030721     C                   Z-ADD     PiVal         VABZNC
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABZNC'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** DCR
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on       AND
137100030721     C                             PiVal<=99999999
137200030721     C                   Z-ADD     PiVal         VABDCR
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABDCR'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900030721     C*** HCR
138000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138100030721     C                   EVAL      PiStr=SkSplitCSV(i)
138200030721     C                   EXSR      CHKNUM
138300030721     C                   IF        PiInt=*on   AND
138400030721     C                             PiVal<=9999
138500030721     C                   Z-ADD     PiVal         VABHCR
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABHCR'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** VMD
139300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiNum=*on
139700030721     C                   Z-ADD     PiVal         VABVMD
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140100030721     C                             + ' ' + 'VABVMD'
140200030721     C                   ENDIF
140300030721     C                   ENDIF
140400030721     C*** ANT
140500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140600030721     C                   EVAL      PiStr=SkSplitCSV(i)
140700030721     C                   EXSR      CHKNUM
140800030721     C                   IF        PiInt=*on        AND
140900030721     C                             PiVal<=999999999
141000030721     C                   Z-ADD     PiVal         VABANT
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABANT'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030721     C*** CMO
141800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141900030721     C                   EVAL      PiStr=SkSplitCSV(i)
142000030721     C                   EXSR      CHKNUM
142100030721     C                   IF        PiInt=*on
142200030721     C                   Z-ADD     PiVal         Num5_0
142300030721     C                   MOVEL(p)  Num5_0        VABCMO
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600030721     C                   EVAL      VABCMO = *zeros
142700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142800030721     C                             + ' ' + 'VABCMO'
142900030721     C                   ENDIF
143000030721     C                   ENDIF
143100040510     C*
143200071120     C* Reperisco i campi del EDIVAT
143300040510     C*** VATNOT
143400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143600040510     C                   ENDIF
143700040510     C*** VATTRC
143800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144000040510     C                   ENDIF
144100060331     C*** VATNOT_A
144200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144400060331     C                   ENDIF
144500060331     C*** VATNOT_B
144600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144800060331     C                   ENDIF
144900060331     C*** VATNOT_E
145000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145200060331     C                   ENDIF
145300110912     C*** VATNOT_I
145400110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
145500110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145600110912     C                   ENDIF
145700110912     C*** VATNOT_J
145800110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
145900110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
146000110912     C                   ENDIF
146100110912     C*** VATNOT_4
146200110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
146300110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
146400110912     C                   ENDIF
146500040510     C*
146600030715     C                   ENDIF
146700030715     C                   EVAL      i = i + 1
146800030715     C                   ENDDO
146900040510     C*
147000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147100040510     C                   IF        VABNSP = *zeros
147200040510     C                   SETON                                        31
147300040510     C                   ENDIF
147400040715     C*
147500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147600040715     C                   IF        VABLNP = *zeros
147700040715     C                   SETON                                        31
147800040715     C                   ENDIF
147900040510     C*
148000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148100040510     C                   IF        VABRMN = *zeros
148200040510     C                   Z-ADD     VABNSP        VABRMN
148300040510     C                   ENDIF
148400040510     C*
148500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148600040510     C                   IF        VABRMA = *blanks
148700040714     C                   MOVEL     VABRMN        VABRMA
148800040510     C                   ENDIF
148900070705     C*
149000070705     C* Considerazioni particolari
149100070705     C*** NZD
149200070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
149300070705     C                   EVAL      VABNZD = *blanks
149400070705     C                   ENDIF
149500070705     C*** CAD
149600070705     C                   IF        VABNZD = *blanks
149700070705     C                   EVAL      PiStr=VABCAD
149800070705     C                   EXSR      CHKNUM
149900070705     C                   IF        PiInt=*on
150000070705     C                   Z-ADD     PiVal         Num5_0
150100070705     C                   MOVEL(p)  Num5_0        VABCAD
150200070705     C                   ELSE
150300070705     C                   SETON                                        32
150400070705     C                   EVAL      VABCAD = *zeros
150500070705     C                   EVAL      vinmsg = %trimr(vinmsg)
150600070705     C                             + ' ' + 'VABCAD'
150700070705     C                   ENDIF
150800070705     C                   ENDIF
150900040128     C*
151000040510     C* Se provincia nn valorizzata la reperisco
151100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151200040510     C                   IF        VABPRD = *blanks
151300040128     C                   CLEAR                   TISI95DS
151400040128     C                   EVAL      I95TCN = '3'
151500040128     C                   Z-ADD     datcor        I95DAT
151600040128     C                   EVAL      I95NAR = VABNZD
151700040128     C                   EVAL      I95CAP = VABCAD
151800040128     C                   EVAL      I95LOC = VABLOD
151900040128     C                   CALL      'TISI95R'
152000040128     C                   PARM                    TISI95DS
152100040128     C                   EVAL      VABPRD = O95PRV
152200040510     C                   ENDIF
152300040113     C*
152400040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
152500040113     C                   IF        VABCAS > *zeros
152600040119     C                   EVAL      VABVCA = 'EUR'
152700040119     C                   IF        VABCBO = '1'
152800040113     C                   EVAL      VABCBO = '4'
152900040119     C                   ENDIF
153000040119     C                   IF        VABCBO = '2'
153100040119     C                   EVAL      VABCBO = '6'
153200040119     C                   ENDIF
153300040113     C                   ENDIF
153400020204     C*
153500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153600020204     C                   EXSR      CHKIMPDIV
153700030715     C                   ENDIF
153800020204     C*
153900010607     C                   ENDSR
154000010607     C*----------------------------------------------------*
154100010601
154200020204
154300020204     C*----------------------------------------------------*
154400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154500020204     C*----------------------------------------------------*
154600020204     C     CHKIMPDIV     BEGSR
154700020204     C*
154800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154900020204     C                   Z-ADD     *zeros        wrkDec            9 9
155000020204     C*
155100020204     C* Come prima cosa effettuo considerazioni sulla divisa
155200020204     C                   IF        vabIAS > *zeros
155300020204     C                   IF        vabVAS <> 'EUR'
155400020204     C                   EVAL      vabVAS =  'ITL'
155500020204     C                   ENDIF
155600020204     C                   ENDIF
155700020204     C*
155800020204     C                   IF        vabCAS > *zeros
155900020204     C                   IF        vabVCA <> 'EUR'
156000020204     C                   EVAL      vabVCA =  'ITL'
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C                   IF        vabVMD > *zeros
156500020204     C                   IF        vabVAD <> 'EUR'
156600020204     C                   EVAL      vabVAD =  'ITL'
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157100020204     C                   Z-ADD     vabIAS        wrkDec
157200020204     C                   IF        wrkDec > *zeros
157300020204     C                   IF        vabVAS = 'ITL'
157400020204     C                   EVAL      vabIAS = *zeros
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157900020204     C                   Z-ADD     vabCAS        wrkDec
158000020204     C                   IF        wrkDec > *zeros
158100020204     C                   IF        vabVCA = 'ITL'
158200020204     C                   EVAL      vabCAS = *zeros
158300020204     C                   ENDIF
158400020204     C                   ENDIF
158500020204     C*
158600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158700020204     C                   Z-ADD     vabVMD        wrkDec
158800020204     C                   IF        wrkDec > *zeros
158900020204     C                   IF        vabVAD = 'ITL'
159000020204     C                   EVAL      vabVMD = *zeros
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C                   ENDSR
159500020204     C***
159600020204
159700010330
159800010330
159900010330     C*----------------------------------------------------*
160000010330     C*  CONTROLLO NUMERICITA' CAMPI
160100010330     C*----------------------------------------------------*
160200010330     C     CHKNUM        BEGSR
160300010330     C*
160400010606     C                   IF        PiDecChr = *blanks
160500030715     C                   EVAL      PiDecChr = CharNUM
160600010606     C                   ENDIF
160700010606     C*
160800010606     C                   CALL(e)   'ISNUMERIC'
160900010330     C                   PARM                    PiStr            30
161000010606     C                   PARM                    PiDecChr          1
161100010330     C                   PARM      *ZEROS        PiVal            30 9
161200010330     C                   PARM      '0'           PiInt             1
161300010330     C                   PARM      '0'           PiNum             1
161400010330     C                   IF        %error
161500010606     C                   EVAL      PiNum=*off
161600010330     C                   ENDIF
161700010330     C*
161800010330     C                   ENDSR
161900010330     C***
162000010330
162100010601
162200010601
162300010601
162400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
162500010601     C     repfil        BEGSR
162600010601     C*
162700010601     C                   if        invfil = *zeros and
162800010601     C                             depfil > *zeros and
162900010629     C                             (vinflg = *blanks or
163000010629     C                              vinflg = *zeros)
163100010601     C
163200010601     C                   eval      invfil = depfil
163300010601     C                   endif
163400010601     C*
163500010601     C                   if        depfil <> invfil and
163600010601     C                             invfil > *zeros
163700010601     C                   eval      flgMulti = '1'
163800010601     C                   if        vinflg = *blanks
163900010601     C                   add       1             cntNonEl
164000010601     C                   endif
164100010601     C                   endif
164200010601     C*
164300010601     C                   if        vinflg = '2'
164400010601     C                   eval      flgStato = '2'
164500010601     C                   endif
164600010601     C*
164700010601     C                   ENDSR
164800010601     C***
164900010601
165000010601
165100010601
165200010330
165300040119      /TITLE Invio dei dati al punto operativo.
165400040119     C     invio         BEGSR
165500040119     C*
165600071120     C* 1° invio EDIVAT
165700040119     C                   reset                   dscmz
165800070704     C                   move      winvcmz       cmzdst
165900071120     C                   eval      cmzfld = 'EDIVATWR'
166000040119     C                   eval      cmzmbd = vlrhdl
166100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166200040119     C***                if        prmfir = *blanks
166300071120     C                   eval      cmzfla = 'EDIVAT0F'
166400071120     C                   eval      cmzmba = 'EDIVAT0F'
166500040119     C***                else
166600040119     C***                eval      cmzfla = prmfir
166700040119     C***                eval      cmzmba = prmfir
166800040119     C***                endif
166900040119     C                   eval      cmznrr = *zeros
167000040119     C                   move      §ctrokvt      cmznrr
167100040119     C                   eval      cmzlba = vlrfl1
167200040119     C                   call(e)   'TIS711C'
167300040119     C                   parm                    dscmz
167400040119     C                   parm      *blanks       esito
167500040119     C                   if        %error
167600040119     C                             or cmzerr = '1'
167700040119     C                             or esito  = '1'
167800040119     C                   eval      wrkesito = '3'
167900040119     C                   else
168000040119     C*
168100071120     C* 2° invio EDIVAB
168200040119     C                   reset                   dscmz
168300070704     C                   move      winvcmz       cmzdst
168400040119     C                   eval      cmzfld = vlrfou
168500040119     C                   eval      cmzmbd = vlrhdl
168600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168700040119     C***                if        prmfir = *blanks
168800071120     C                   eval      cmzfla = 'EDIVAB0F'
168900071120     C                   eval      cmzmba = 'EDIVAB0F'
169000040119     C***                else
169100040119     C***                eval      cmzfla = prmfir
169200040119     C***                eval      cmzmba = prmfir
169300040119     C***                endif
169400040119     C                   eval      cmznrr = *zeros
169500040119     C                   move      §ctrokvb      cmznrr
169600040119     C                   eval      cmzlba = vlrfl1
169700040119     C                   call(e)   'TIS711C'
169800040119     C                   parm                    dscmz
169900040119     C                   parm      *blanks       esito
170000040119     C                   if        %error
170100040119     C                             or cmzerr = '1'
170200040119     C                             or esito  = '1'
170300040119     C                   eval      wrkesito = '3'
170400040119     C                   endif
170500040119     C                   endif
170600040119     C*
170700040119     C                   ENDSR
170800040119     C***
170900010601
171000010601
171100010601
171200010601
171300010601      /TITLE Invio dei dati al punto operativo.
171400010601     C     opeini        BEGSR
171500010601     C*
171600010601     C* Inizializzo flag e contatori operativi
171700010601     C                   movel     '0'           flgGiro           1
171800010601     C                   movel     '0'           flgMulti          1
171900010601     C                   movel     '1'           flgStato          1
172000010615     C                   movel     '0'           flgOk             1
172100010601     C                   z-add     *zeros        cntNonEl         10 0
172200010601     C                   z-add     *zeros        depfil            3 0
172300010601     C                   z-add     *zeros        invfil            3 0
172400010601     C*
172500010601     C                   ENDSR
172600010601     C***
172700070326
172800070326
172900070326
173000070326
173100070326     C     *pssr         BEGSR
173200070329     C*
173300070329     C                   if        %open(tivin00r)
173400070329     C                   close     tivin00r
173500070329     C                   endif
173600071120     C                   if        %open(edivabwr)
173700071120     C                   close     edivabwr
173800070329     C                   endif
173900071120     C                   if        %open(edivatwr)
174000071120     C                   close     edivatwr
174100070329     C                   endif
174200070326     C*
174300070326     C* Effettuo la chiamata al CLLE preposto
174400071121     C                   call(e)   'TITVEVTC'
174500070326     C                   parm                    parccm
174600070326     C                   parm                    parmbr
174700070326     C                   parm      '2'           paropz
174800070326     C*
174900070326     C                   eval      wrkesito = '2'
175000070404     C*
175100070404     C                   seton                                        LR
175200070326     C*
175300070326     C                   ENDSR     '*CANCL'
175400070326     C***
175500070326
175600070326
175700010330
175800010330
175900000613     C     *inzsr        BEGSR
176000990910     C*
176100990910     C     *entry        plist
176200990920     C                   parm                    tivlrds
176300990921     C                   parm      wrkesito      esito
176400000724     C                   parm                    prmlit
176500000710     C                   parm                    prmfir
176600010330     C*
176700010330     C* CALCOLA LA DATA CORRENTE
176800010330     C                   time                    wn14             14 0
176900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
177000071120     C                   movel     wn14          oracor            6 0          *ORA
177100010330     C                   z-add     wn8           g08dat
177200010330     C                   z-add     *zeros        g08inv
177300010330     C                   movel     '0'           g08err
177400010330     C                   call      'XSRDA8'
177500010330     C                   parm                    wlbda8
177600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
177700000613     C*
177800000613     C                   ENDSR
177900000613     C***
178000990908
178100040122     Otitv92p   E            riepilogo         2
178200990915     O                                              'Upload via Internet'
178300990915     O                                           +1 'Traduzione TIVIN00R -'
178400071120     O                                           +1 'EDIVABWR'
178500990915     O                                           +1 'Testate bolle'
178600990915     O          E            riepilogo   2
178700990915     O                       wrkdata
178800990915     O                       wrkora              +1
178900990915     O                       procname            +1
179000990915     O          E            riepilogo   2
179100990915     O                                              'Cliente..................:'
179200010605     O                       VABCCM        z     +1
179300990915     O          E            riepilogo   2
179400990920     O                                              'Riferimento Strategi.....:'
179500990920     O                       vlrhdl              +1
179600990915     O          E            riepilogo   2
179700990915     O                                              'Giusti...................:'
179800040119     O                       §CTROKVB      2   +  1
179900990915     O          E            riepilogo   2
180000990915     O                                              'Sbagliati e corretti.....:'
180100971022     O                       §CTRMO        2   +  1
180200990915     O          E            riepilogo   2
180300990915     O                                              'Sbagliati e scartati.....:'
180400971022     O                       §CTRNO        2   +  1
180500000613
180600040122     Otitv92ps  E            testdett          2
180700000613     O                                              'Upload via Internet'
180800000613     O                                           +1 'Traduzione TIVIN00R -'
180900071120     O                                           +1 'EDIVABWR'
181000000613     O                                           +1 'Testate bolle'
181100000616     O          E            testdett    3
181200000613     O                                           +2 'N° rec'
181300000613     O                                           +3 'Anteprima contenuto'
181400000616     O          E            rigadett    2
181500000613     O                       rrnum               +2
181600000621     O                       recko               +3
181700000616     O          E            findett     2
181800000613     O                       wrkdata
181900000613     O                       wrkora              +1
182000000613     O                       procname            +1
182100000616     O          E            findett     2
182200000613     O                                              'Cliente..................:'
182300010605     O                       VABCCM        z     +1
182400000616     O          E            findett     2
182500000613     O                                              'Riferimento Strategi.....:'
182600000613     O                       vlrhdl              +1
182700000616     O          E            findett     2
182800000613     O                                              'Giusti...................:'
182900040119     O                       §CTROKVB      2   +  1
183000000616     O          E            findett     2
183100000613     O                                              'Sbagliati e corretti.....:'
183200000613     O                       §CTRMO        2   +  1
183300000616     O          E            findett     2
183400000613     O                                              'Sbagliati e scartati.....:'
183500000613     O                       §CTRNO        2   +  1
183600000613** CMD - COMANDI CL
183700040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
183800040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
183900040122DLTOVR FILE(TITV92P TITV92PS) LVL(*)
184000010606
184100000613
184200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184300040510
184400040122
184500040113
